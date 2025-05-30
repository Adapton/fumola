use fumola::check::{assert_parse as assert_to, assert_roundtrip as assert_};

fn assert_parse_ok(s: &str) {
    fumola::check::parse(s).expect("assert_parse_ok");
}

fn assert_parse_err(s: &str) {
    match fumola::check::parse(s) {
        Err(_e) => {}
        Ok(_) => {
            unreachable!("expected syntax error, not an 'OK parse'.")
        }
    }
}
#[test]
fn test_prim_types() {
    let d = fumola::check::parse("type t = Nat8").expect("assert_parse_ok");
    match &d.vec.get(0).expect("get parsed decl").0 {
        fumola::ast::Dec::Type(_, _, t) => match &t.0 {
            fumola::ast::Type::Prim(_) => {}
            t => unreachable!("expected Type::Prim, not {:?}", t),
        },
        d => unreachable!("expected Dec::Type, not {:?}", d),
    }
}

#[test]
fn test_dot_dot() {
    assert_parse_ok("foo . 0 . 1");
    assert_parse_ok("foo.0 . 1");
    assert_parse_ok("foo . 0.1");
    assert_parse_ok("foo.0.1");
    assert_parse_ok("foo.0.1.0.1.0.1");
}

#[test]
fn test_type_decl() {
    assert_parse_ok("type T = ()");
    assert_parse_ok("type T = {}"); // note that "{} cannot produce type ()".
    assert_parse_ok("type T = {#banana}");
    assert_parse_ok("type T = {#banana : Nat}");
    assert_parse_ok("type T = {banana : Nat}");
    assert_parse_ok("type T = {banana : Nat; apple : Text}");
}

#[test]
fn test_actor() {
    assert_parse_ok("actor { }");
}

#[test]
fn test_generic_identity_function() {
    assert_parse_ok("func f<T>(x : T) : T { x }");
}

#[test]
fn test_generic_pair_function() {
    assert_parse_ok("func f<X, Y>(x : X, y : Y) : (X, Y) { (x, y) }");
}

#[test]
fn test_generic_record_function() {
    assert_parse_ok("func f<X, Y>(x : X, y : Y) : {#first : X; #second : Y} { (x, y) }");
}

#[test]
fn test_generic_application() {
    assert_parse_ok("f<Nat>(3)");
    assert_parse_ok("f<Nat, Text>(3, \"balloons\")");
}

#[test]
fn test_query_func() {
    assert_parse_ok("actor { public query func f() : async Nat { 1 } }");
}

#[test]
fn test_shared_func() {
    assert_parse_ok("actor { public shared func f() : async Nat { 1 } }");
}

#[test]
fn test_shared_query_func() {
    assert_parse_ok("actor { public shared func f() : async Nat { 1 } }");
}

#[test]
fn test_query_shared_func_err() {
    assert_parse_err("actor { public query shared func f() : async Nat { 1 } }");
}

#[test]
fn test_option() {
    assert_("?1");
    assert_("?()");
    assert_("?(1)");
    assert_("?(1, 2,)");
    assert_parse_ok("?{ }");
    assert_("?#apple");
}

#[test]
fn test_ids() {
    assert_("x");
    assert_("X");
    assert_("xxx_");
    assert_("xXx_01");
    assert_("xxx_01_xxX");
    assert_("_x"); // valid in OCaml fumola

    // 'let' as a variable -- this is not a legal program, but we want a good parse error, so parse it as a variable.
    // currently results in a parse error
    //assert_("let");
}

#[test]
fn test_chars() {
    assert_("'h'");
}

#[test]
fn test_strings() {
    assert_("\"hello\"");
}

#[test]
fn test_paren() {
    assert_("(1)");
    assert_("(\"hello\")");
}

#[test]
fn test_tuples() {
    assert_("()");
    if false {
        // to do 2022-08-05.
        assert_("(,)");
    }
    assert_("(1,)");
    assert_("(1, 2)");
    assert_("(1, 2,)");

    assert_("(1, 2, 3)");
    assert_("(1, 2, 3,)");

    // current trailing delimiter behavior
    assert_to("(1, )", "(1,)");
}

#[test]
fn test_nats() {
    assert_("0");
    assert_("123");
}

#[test]
fn test_ints() {
    assert_("-0");
    assert_("-123");
}

#[test]
fn test_unop() {
    assert_("-0");
    assert_("+0");
    assert_("^0");
    assert_to("- 0", "-0");
    assert_to("+ 0", "+0");
    assert_to("^ 0", "^0");
}

#[test]
fn test_floats() {
    assert_("0.");
    assert_("-0.0");
    assert_("-123.123");
    assert_("1_000_000");
    assert_("1_2.3_4");
    assert_("1_2.3_4e5_6");
}

#[test]
fn test_bools() {
    assert_("true");
    assert_("false");
}

#[test]
fn test_unary_operators() {
    assert_("-0");
    assert_("+0");
    assert_("not true");
}

#[test]
fn test_bang() {
    assert_to("null !", "null!");
}

#[test]
fn test_binary_operators() {
    assert_("0 * 0");
    assert_("0 + 0");
    assert_("0 + 0 * 0 + 0");
    assert_("0 + 0 * 0 | 0 | 0 * 0 + 0");
    assert_to("2-1", "2 - 1");
    /*
    assert_("0 - 0");
    assert_("0 / 0");
    assert_("0 % 0");
    assert_("0 ** 0");
    assert_("0 ^ 0"); // Xor or Cat ???
    assert_("0 << 0");
    assert_("0 >> 0");
    assert_("0 <<> 0");
    assert_("0 <>> 0");
    assert_("0 +% 0");
    assert_("0 *% 0");
    assert_("0 **% 0");
    assert_("0 = 0");
    assert_("0 != 0");
    assert_("0 < 0");
    assert_("0 > 0");
    assert_("0 >= 0");
    assert_("0 <= 0");
     */
}

#[test]
fn test_return() {
    assert_("return 0");
    assert_("return (0, 1)");
}

#[test]
fn test_const_array() {
    assert_("[]");
    assert_("[1]");
    assert_("[1,]");
    assert_("[1, 2]");
    assert_("[1, 2,]");
}

#[test]
fn test_decs() {
    assert_("x;");
    assert_("x; x");
    assert_("x; x;");
}

#[test]
fn test_var_array() {
    assert_("[var ]");
    assert_("[var 1]");
    assert_("[var 1,]");
    assert_("[var 1, 2]");
    assert_("[var 1, 2,]");
}

#[test]
fn test_let_var() {
    assert_("let x = 0; x");
    assert_parse_ok("let x : Int = 0; x");
    assert_parse_ok("var x = 0; x");
    assert_parse_ok("var x : Int = 0; x");
}

#[test]
fn test_do_block() {
    assert_("do { let x = 0; x }");
    assert_("let y = do { let x = 0; x }; y");
    assert_("var x = do { var y = 0; x }; x");
}

#[test]
fn test_variant() {
    assert_("#banana");
    assert_("#banana 0");

    if true {
        // to do -- fix formatter, then use else branch only.
        assert_("#banana (0)");
        assert_("#banana (#apple)");
    } else {
        assert_("#banana(0)");
        assert_("#banana(#apple)");
    }
}

#[test]
fn test_record() {
    assert_parse_ok("{ }");
    assert_parse_ok("{ x }");
    assert_parse_ok("{ x : Nat }");
    assert_parse_ok("{ x = 3 }");
    assert_parse_ok("{ x = 3; }");
    assert_parse_ok("{ x with y = 3 }");
    assert_parse_ok("{ x and y with z = 3 }");
    assert_parse_ok("{ f x and f y with z = 3 }");
    assert_parse_ok("{ x : Nat = 3; }");
    assert_parse_ok("{ x : Nat = 3; y = #apple }");
    assert_parse_ok("{ var x : Nat = 3; var y = #apple }");
    assert_parse_ok("{ var x = 3; var y = #apple }");
    assert_parse_ok("{ var x = 3; y }");
    assert_parse_ok("{ x; y }");
    assert_parse_ok("{ x with z = 3}");
    assert_parse_ok("{ f x with z = 3}");
    assert_parse_ok(
        "
      func f () : { x : Nat } = { x = 3 };
      func g () : { y : Nat } = { y = 3 };
      let (x, y) = ((), ());
      { f x and g y with z = 3 };
    ",
    );
}

#[test]
fn test_assign() {
    assert_("x := 3");
}

#[test]
fn test_bin_assign() {
    assert_parse_ok("x += 3");
    assert_parse_ok("x -= 3");
    assert_parse_ok("x *= 3");
    assert_parse_ok("x /= 3");
    assert_parse_ok("x %= 3");
    assert_parse_ok("x **= 3");
    assert_parse_ok("x &= 3");
    assert_parse_ok("x |= 3");
    assert_parse_ok("x ^= 3");
    assert_parse_ok("x <<= 3");
    assert_parse_ok("x >>= 3");
    assert_parse_ok("x <<>= 3");
    assert_parse_ok("x <>>= 3");
    assert_parse_ok("x +%= 3");
    assert_parse_ok("x -%= 3");
    assert_parse_ok("x *%= 3");
    assert_parse_ok("x **%= 3");
    assert_parse_ok("x #= 3");
}

#[test]
fn test_if() {
    assert_("if true 1 else 2");
    assert_("if true { 1 } else { 2 };");
    assert_to("if true { }", "if true { }");
}

#[test]
fn test_seq() {
    assert_("ignore 0; ignore 1; 0");
    assert_("ignore 0; ignore 1; 0;");
}

#[test]
fn test_ignore() {
    assert_("ignore 1");
}

#[test]
fn test_switch() {
    assert_("switch 0 { }");
    assert_("switch 0 { case _ 0 }");
    assert_("switch 0 { case _ 0; }");
    assert_("switch 0 { case (_) 0 }");
    assert_("switch 0 { case (_,) 0 }");
    assert_("switch 0 { case (_, _) 0 }");
    assert_("switch 0 { case (_, _,) 0 }");
    assert_("switch (#apple) { case (#apple) 1 }");
    assert_("switch (#apple) { case (#apple) 1; }");
}

#[test]
fn test_record_proj() {
    assert_("x.foo");
    assert_to("x . foo", "x.foo");
}

#[test]
fn test_tuple_proj() {
    assert_parse_ok("x.0");
    assert_parse_ok("x . 0");
}

#[test]
fn test_array_index() {
    assert_("x[0]");
    assert_to("x [ 0 ]", "x[0]");
}

#[test]
fn test_call() {
    // to do -- handle type instantiations, via <(Type,)+> syntax
    assert_to("f 0", "f(0)");
    assert_to("f (0, 1)", "f((0, 1))");
    assert_to("0 0", "0(0)");
    assert_to("0 f", "0(f)");
    assert_to("(0) (f)", "(0)((f))");
    assert_to("f(x)", "f((x))");
    assert_to("(f)(x)", "(f)((x))");
    assert_to("(f)x", "(f)(x)");
}

#[test]
fn test_line_comments() {
    assert_to("//\n0//", "0");
    assert_to("//0\n0//0\n//0", "0");
    assert_to("0//", "0");
    assert_to("// /*\n0", "0");
    assert_to("// */\n0", "0");
    assert_to("\"\"//\"\"", "\"\"");
    assert_to("'a'//'b'", "'a'");
}

#[test]
fn test_block_comments() {
    assert_to("/*(*/", "");
    assert_to("#/**/a", "#a");
    assert_to("#/***/a", "#a");
    assert_to("#/****/a", "#a");
    assert_to("#/* */a", "#a");
    assert_to("#/** */a", "#a");
    assert_to("#/* **/a", "#a");
    assert_to("#/** **/a", "#a");
    assert_to("#/* * */a", "#a");
    assert_to("#/* /**/ */a", "#a");
    assert_to("#/* /* */ */a", "#a");
    assert_to("#/* /** */ */a", "#a");
    assert_to("#/* /* **/ */a", "#a");
    assert_to("#/* // */a", "#a");
    assert_to("#/*\n*\n*\n*/a", "#a");
    assert_to("let///* */\na = 0", "let a = 0");
    assert_to("/*\"*/\"\"", "\"\"");
    assert_to("\"\"/*\"*/", "\"\"");
    assert_to("/*\"*/\"\"/*\"*/", "\"\"");
    assert_to("\"/**/\"", "\"/**/\"");
    assert_to("\"/*\" # \"*/\"", "\"/*\" # \"*/\"");
    assert_to("\"/*//\\\\\\\"\"", "\"/*//\\\\\\\"\"");
    assert_to("\"/**//**/\\\\\\\"\"", "\"/**//**/\\\\\\\"\"");
}

// #[test]
// fn test_source_comments() {
//     //use fumola::ast::{Dec, Exp, Loc, Source};
//     use fumola::check::parse;
//     let ast = parse("//\n/*a*/a").unwrap();
//     println!("{:?}", ast);

//     // 20221004 this will fail until we fill in "TODO" with the right answer.OA
//     assert_eq!(
//         format!("{:?}", ast.vec[0]),
//         "<Exp(<Var(\"a\")@8..9 @ 2:6>)@8..9 @ 2:6>"
//     );
// }

#[test]
fn test_multiline_text() {
    assert_to("/*\"*/ \"~\n\n~\" /*\"*/", "\"~\n\n~\"");
    // assert_to("\"A\nB\"", "\"A\\nB\""); // Possible corner case depending on the final OCaml implementation
}

#[test]
fn test_misc() {
    assert_to(r#"'\"'; //abc"#, r#"'\"';"#);
}
