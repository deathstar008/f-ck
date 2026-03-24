open System

// ===============
// 1. Tokens
// ===============

type Token =
    | LET
    | IF
    | THEN
    | ELSE
    | FUN
    | TRUE
    | FALSE
    | IDENT of string
    | INT of int
    | STRING of string
    | EQ          // =
    | ARROW       // ->
    | LBRACK      // [
    | RBRACK      // ]
    | LPAREN      // (
    | RPAREN      // )
    | COMMA       // ,
    | PIPEGT      // |>
    | PLUS
    | MINUS
    | STAR
    | SLASH
    | GT
    | LT
    | EOF

// ===============
// 2. Lexer
// ===============

let isLetter c = Char.IsLetter c || c = '_'
let isDigit c = Char.IsDigit c

let keywords =
    dict [
        "let", LET
        "if", IF
        "then", THEN
        "else", ELSE
        "fun", FUN
        "true", TRUE
        "false", FALSE
    ]

let tokenize (input:string) =
    let len = input.Length
    let mutable pos = 0
    let peek () = if pos < len then Some input[pos] else None
    let advance () = pos <- pos + 1

    let rec skipWhitespace () =
        match peek () with
        | Some c when Char.IsWhiteSpace c -> advance (); skipWhitespace ()
        | _ -> ()

    let readWhile pred =
        let start = pos
        while pos < len && pred input[pos] do
            pos <- pos + 1
        input.Substring(start, pos - start)

    let readNumber () =
        let s = readWhile isDigit
        INT (int s)

    let readIdentOrKeyword () =
        let s = readWhile (fun c -> isLetter c || isDigit c || c = '_')
        match keywords.TryGetValue s with
        | true, tok -> tok
        | _ -> IDENT s

    let readString () =
        advance () // skip opening "
        let start = pos
        while pos < len && input[pos] <> '"' do
            pos <- pos + 1
        let s = input.Substring(start, pos - start)
        if peek () = Some '"' then advance ()
        STRING s

    let rec loop acc =
        skipWhitespace ()
        match peek () with
        | None -> List.rev (EOF :: acc)
        | Some c ->
            match c with
            | '=' -> advance (); loop (EQ :: acc)
            | '-' ->
                advance ()
                match peek () with
                | Some '>' -> advance (); loop (ARROW :: acc)
                | _ -> loop (MINUS :: acc)
            | '+' -> advance (); loop (PLUS :: acc)
            | '*' -> advance (); loop (STAR :: acc)
            | '/' -> advance (); loop (SLASH :: acc)
            | '>' -> advance (); loop (GT :: acc)
            | '<' -> advance (); loop (LT :: acc)
            | '[' -> advance (); loop (LBRACK :: acc)
            | ']' -> advance (); loop (RBRACK :: acc)
            | '(' -> advance (); loop (LPAREN :: acc)
            | ')' -> advance (); loop (RPAREN :: acc)
            | ',' -> advance (); loop (COMMA :: acc)
            | '|' ->
                advance ()
                match peek () with
                | Some '>' -> advance (); loop (PIPEGT :: acc)
                | _ -> failwith "Unexpected '|'"
            | '"' -> 
                let strTok = readString ()
                loop (strTok :: acc)
            | c when isDigit c ->
                let numTok = readNumber ()
                loop (numTok :: acc)
            | c when isLetter c ->
                let idTok = readIdentOrKeyword ()
                loop (idTok :: acc)
            | _ ->
                failwithf "Unexpected character: %c" c
    loop []

// ===============
// 3. AST
// ===============

type Expr =
    | EInt of int
    | EBool of bool
    | EString of string
    | EVar of string
    | ELet of string * Expr * Expr
    | EIf of Expr * Expr * Expr
    | EFun of string * Expr
    | EApp of Expr * Expr
    | EList of Expr list
    | EBinOp of string * Expr * Expr
    | EPipe of Expr * Expr

// ===============
// 4. Parser
// ===============

type Parser(tokens: Token list) =
    let mutable pos = 0
    let toks = List.toArray tokens
    let current () = if pos < toks.Length then toks[pos] else EOF
    let advance () = pos <- pos + 1
    let expect t =
        if current () = t then advance ()
        else failwithf "Expected %A but got %A" t (current ())

    // program = { statement }
    member this.ParseProgram() =
        let rec loop acc =
            match current () with
            | EOF -> List.rev acc
            | _ ->
                let stmt = this.ParseExpr()
                loop (stmt :: acc)
        loop []

    // expr = ifExpr | funExpr | binExpr
    member this.ParseExpr() =
        match current () with
        | IF -> this.ParseIf()
        | FUN -> this.ParseFun()
        | _ -> this.ParseBinExpr()

    member this.ParseIf() =
        expect IF
        let cond = this.ParseExpr()
        expect THEN
        let thenExpr = this.ParseExpr()
        expect ELSE
        let elseExpr = this.ParseExpr()
        EIf(cond, thenExpr, elseExpr)

    member this.ParseFun() =
        expect FUN
        let arg =
            match current () with
            | IDENT name -> advance (); name
            | _ -> failwith "Expected identifier after 'fun'"
        expect ARROW
        let body = this.ParseExpr()
        EFun(arg, body)

    // binExpr = pipeExpr ( ( + | - | * | / | > | < ) pipeExpr )*
    member this.ParseBinExpr() =
        let rec loop left =
            match current () with
            | PLUS  -> advance (); let right = this.ParsePipeExpr() in loop (EBinOp("+", left, right))
            | MINUS -> advance (); let right = this.ParsePipeExpr() in loop (EBinOp("-", left, right))
            | STAR  -> advance (); let right = this.ParsePipeExpr() in loop (EBinOp("*", left, right))
            | SLASH -> advance (); let right = this.ParsePipeExpr() in loop (EBinOp("/", left, right))
            | GT    -> advance (); let right = this.ParsePipeExpr() in loop (EBinOp(">", left, right))
            | LT    -> advance (); let right = this.ParsePipeExpr() in loop (EBinOp("<", left, right))
            | _ -> left
        let first = this.ParsePipeExpr()
        loop first

    // pipeExpr = appExpr ( "|>" appExpr )*
    member this.ParsePipeExpr() =
        let rec loop left =
            match current () with
            | PIPEGT ->
                advance ()
                let right = this.ParseAppExpr()
                loop (EPipe(left, right))
            | _ -> left
        let first = this.ParseAppExpr()
        loop first

    // appExpr = primaryExpr { primaryExpr }
    member this.ParseAppExpr() =
        let rec collect acc =
            match current () with
            | INT _ | TRUE | FALSE | STRING _ | IDENT _ | LBRACK | LPAREN ->
                let e = this.ParsePrimary()
                collect (acc @ [e])
            | _ -> acc
        let parts = collect []
        match parts with
        | [] -> failwith "Expected expression"
        | first :: rest ->
            rest |> List.fold (fun f arg -> EApp(f, arg)) first

    // primaryExpr
    member this.ParsePrimary() =
        match current () with
        | INT n -> advance (); EInt n
        | TRUE -> advance (); EBool true
        | FALSE -> advance (); EBool false
        | STRING s -> advance (); EString s
        | IDENT name ->
            advance ()
            // Could be start of let-binding if followed by '=' and 'let' is not present.
            // But we keep let as a top-level construct via ELet inside expressions:
            // let x = expr in expr  (we'll encode as ELet(x, expr, body))
            // For simplicity, we treat 'let' as keyword only.
            EVar name
        | LBRACK ->
            advance ()
            let rec elems acc =
                match current () with
                | RBRACK -> advance (); List.rev acc
                | _ ->
                    let e = this.ParseExpr()
                    match current () with
                    | COMMA -> advance (); elems (e :: acc)
                    | RBRACK -> advance (); List.rev (e :: acc)
                    | _ -> failwith "Expected ',' or ']' in list"
            let items = elems []
            EList items
        | LPAREN ->
            advance ()
            let e = this.ParseExpr()
            expect RPAREN
            e
        | LET ->
            // let x = expr in expr  (we'll parse as ELet(x, expr, body))
            advance ()
            let name =
                match current () with
                | IDENT n -> advance (); n
                | _ -> failwith "Expected identifier after 'let'"
            // Optional function-style: let f x = expr
            let rec collectArgs acc =
                match current () with
                | IDENT n -> advance (); collectArgs (acc @ [n])
                | _ -> acc
            let args = collectArgs []
            expect EQ
            let valueExpr = this.ParseExpr()
            // For simplicity, we treat "let ... = ..." as binding in an implicit body:
            // we require a following "in" style by just letting the rest of the program be the body.
            // But to keep it expression-oriented, we’ll parse:
            // let x = expr
            // expr2
            // as ELet(x, expr, expr2)
            // Here, we just return a function if there are args.
            let valueExpr =
                match args with
                | [] -> valueExpr
                | _ ->
                    // curry: let f x y = body  => let f = fun x -> fun y -> body
                    List.foldBack (fun arg acc -> EFun(arg, acc)) args valueExpr
            // After a let-binding in expression position, we parse the body:
            let body = this.ParseExpr()
            ELet(name, valueExpr, body)
        | _ ->
            failwithf "Unexpected token in primary: %A" (current ())

// ===============
// 5. Evaluator
// ===============

type Value =
    | VInt of int
    | VBool of bool
    | VString of string
    | VList of Value list
    | VFun of (Value -> Value)

type Env = Map<string, Value>

let rec eval (env:Env) (expr:Expr) : Value =
    match expr with
    | EInt n -> VInt n
    | EBool b -> VBool b
    | EString s -> VString s
    | EVar name ->
        match env.TryFind name with
        | Some v -> v
        | None -> failwithf "Unbound variable: %s" name
    | ELet(name, valueExpr, body) ->
        let v = eval env valueExpr
        let env' = env.Add(name, v)
        eval env' body
    | EIf(cond, thenExpr, elseExpr) ->
        match eval env cond with
        | VBool true -> eval env thenExpr
        | VBool false -> eval env elseExpr
        | _ -> failwith "Condition must be bool"
    | EFun(arg, body) ->
        VFun (fun v ->
            let env' = env.Add(arg, v)
            eval env' body)
    | EApp(fExpr, argExpr) ->
        let fVal = eval env fExpr
        let argVal = eval env argExpr
        match fVal with
        | VFun f -> f argVal
        | _ -> failwith "Attempted to call non-function"
    | EList items ->
        items |> List.map (eval env) |> VList
    | EBinOp(op, left, right) ->
        let lv = eval env left
        let rv = eval env right
        match op, lv, rv with
        | "+", VInt a, VInt b -> VInt (a + b)
        | "-", VInt a, VInt b -> VInt (a - b)
        | "*", VInt a, VInt b -> VInt (a * b)
        | "/", VInt a, VInt b -> VInt (a / b)
        | ">", VInt a, VInt b -> VBool (a > b)
        | "<", VInt a, VInt b -> VBool (a < b)
        | _ -> failwithf "Unsupported binary op or types: %s" op
    | EPipe(left, right) ->
        // left |> right  => right left
        eval env (EApp(right, left))

let rec valueToString v =
    match v with
    | VInt n -> string n
    | VBool b -> string b
    | VString s -> "\"" + s + "\""
    | VList xs ->
        xs
        |> List.map valueToString
        |> String.concat ", "
        |> sprintf "[%s]"
    | VFun _ -> "<fun>"

// ===============
// 6. REPL + samples
// ===============

let run source =
    let tokens = tokenize source
    let parser = Parser(tokens)
    let exprs = parser.ParseProgram()
    let env = Map.empty
    let mutable last = VInt 0
    for e in exprs do
        last <- eval env e
    last

[<EntryPoint>]
let main _ =
    printfn "F#ck REPL (Ctrl+C to exit)"
    while true do
        Console.Write("> ")
        let line = Console.ReadLine()
        if not (isNull line) && line.Trim() <> "" then
            try
                let v = run line
                printfn "%s" (valueToString v)
            with ex ->
                printfn "Error: %s" ex.Message
    0
