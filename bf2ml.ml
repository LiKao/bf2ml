type ch = Char of char | Eof;;

let rec indent n text= 
   if n == 0 then
       text
   else
      "   " ^ indent (n-1) text
   ;;

let rec parse depth ch =
   let i = ch () in
      match i with
         Char '<' -> indent depth ("do_lft ==>\n" ^ (parse depth ch) )
      |  Char '>' -> indent depth ("do_rgt ==>\n" ^ (parse depth ch))
      |  Char '+' -> indent depth ("do_inc ==>\n" ^ (parse depth ch))
      |  Char '-' -> indent depth ("do_dec ==>\n" ^  (parse depth ch))
      |  Char '.' -> indent depth ("do_put ==>\n" ^ (parse depth ch))
      |  Char ',' -> indent depth ("do_get ==>\n" ^ (parse depth ch))
      |  Char '?' -> indent depth ("do_dbg ==>\n" ^ (parse depth ch))
      |  Char '[' -> 
         let body = parse (depth+1) ch in 
         indent depth "do_lop (\n"  ^ body ^ indent depth ")      ==>\n" ^ parse depth ch
      |  Char ']' -> if depth!=0 then indent depth "return\n" else failwith "Unmatched ]"
      |  Char _ -> parse depth ch
      |  Eof -> if  depth != 0 then failwith "Unmatched [" else indent depth "return"
   ;;
  
let ic = open_in Sys.argv.(1);;
print_string ("open Bflib;;\n\n\nexec (\n" ^ parse 0
   (fun _ -> 
      try
         Char (input_char ic)
      with End_of_file ->
         Eof
   ) ^ ");;\n");;