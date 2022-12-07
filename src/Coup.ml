

(** Coups *)
let a = 1;;
let a = a+1;;

let a = ref 0;;
print_int !a;


for i = 0 to 7 do

  for k = 0 to 6 do
    print_int  i;
    print_int k;

  done;
done;