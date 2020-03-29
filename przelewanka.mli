val przelewanka : (int * int) array -> int 
(** for the given array of pairs [|(x1,y1); (x2,y2); ...; (xn, yn)|],
    where xi is volume and yi expected state of each mug, using given operations:
    - pouring one of the mugs to the brim;
    - pouring out all content from one of the mugs;
    - pouring the content of one mug to the another one untill its filling up, 
      or to the brim, liquid not poured is left over in the first mug;
    determines the minimal number of these operations
    needed to achieve the expected state or returns -1 if it is not possible **)
