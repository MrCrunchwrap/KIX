grammar kix ; 

parser parse :: Root_c
{
  kix; 
}

function main 
IOVal<Integer> ::= largs::[String] ioin::IO
{
  local attribute args :: String;
  args = implode(" ", largs);

  production attribute text :: IOVal<String>;
  text = readFile(args, ioin);

  local attribute result :: ParseResult<Root_c>;
  result = parse(text.iovalue, args);

  local attribute r_cst :: Root_c ;
  r_cst = result.parseTree ;

  local attribute r_ast :: Root ;
  r_ast = r_cst.ast_Root ;

  local attribute print_success :: IO;
  print_success = 
    writeFile("Output.txt", "CST pretty print: " ++ r_cst.pp ++
           "\n\n" ++ 
           "AST pretty print: " ++ r_ast.pp ++
           "\n\n" ++
           foldl (stringConcat, "", r_ast.errors) ++ "\n\n"
           , ioin );

  local attribute print_failure :: IO;
  print_failure =
    print("Encountered a parse error:\n" ++ result.parseErrors ++ "\n", ioin);

  return ioval(if result.parseSuccess then print_success else print_failure,
               0);
}




