(* Ababacar Sadikh GUEYE *)
type coup   = int;;
type case   = |Rouge|Trou|Jaune;;
type joueur = |Homme|Machine;;
type plateau  = case array array;;
type colonne  = {plein:bool; nbpp:int};;
type position = {plateau:plateau; colonnes:colonne array};;
type compteur = {mutable homme:int; mutable machine:int};;
type cas = {configuration:int; joueur:bool; suivant:int; gagne:bool};;
type best_coup = {bcoup:coup; valeur:int};;

open Graphics

let mine= -1073741824 and maxe=1073741823;;

let int_of_case = function
|Rouge -> 0
|Trou  -> 1
|Jaune -> 2
;;

let case_of_joueur = function
|Homme   -> Jaune
|Machine -> Rouge
;;

exception Cnv  of string;;
exception Chgt of string;;
exception Gagne of coup;; (*maintenant c'est gagné si on joue ce coup le*)
exception Gagnant;; (*ce plateau est gagnant*)

open_graph "";;
let gauche = (size_x () -420)/2 and bas= (size_y () -360)/2;;

(*la definition de fleche :*)
clear_graph ();
moveto 30 60;
lineto 30 25;
moveto 30 00;
lineto 15 25;
lineto 45 25;
lineto 30 00;;
let fleche = get_image 0 0 60 60;;
clear_graph ();;

let (char_w,char_h) = text_size "X";;

(******************************************************************)
(*entrees-sorties*)

let set_couleur case = match case with
|Rouge -> set_color red
|Jaune  -> set_color yellow
|Trou  -> set_color white
;;

let draw_case case coord =
set_couleur case;
let (x,y)=coord in
fill_circle (gauche + 60*x + 30) (bas + 60*y + 30) 25
;;

let draw_plateau plateau =
set_color blue; fill_rect gauche bas 420 360;
for i=0 to 6 do
  for j=0 to 5 do
    draw_case plateau.(i).(j) (i,j);
  done;
done
;;


let designe bool coup =
if bool then draw_image fleche (gauche + 60*coup) (bas + 360)
else (set_color background; fill_rect (gauche + 60*coup) (bas + 360) 60 60)
;;

let coup_of_as abscisse_souris =
if (abscisse_souris >= gauche) & (abscisse_souris <= gauche+419)
then (abscisse_souris -gauche) /60
else raise (Cnv "hors plateau")
;;

let get_coup_homme () =
let pasdecide = ref true
  and old_coup = ref (-1) (*hors plateau e l'initialisation*)
  and coup = ref (-1) in
while !pasdecide do
  let status = wait_next_event [Mouse_motion; Button_down; Key_pressed] in
  (try coup:= coup_of_as status.mouse_x with
    |Cnv "hors plateau" -> ());
  if status.keypressed = true then raise (Chgt "")
  else if status.button = true then
    try (pasdecide:=false; coup:= coup_of_as status.mouse_x) with
    |Cnv "hors plateau" -> raise (Cnv "Cliquer sur une colonne !")
  (*derniere possibilite : on a seulement bouge la souris :*)
  else if !old_coup <> !coup then
    begin designe false !old_coup; designe true !coup;
      old_coup:= !coup;
    end;
done;
designe false !coup;
!coup
;;


let print_score compteur =
set_color black;
moveto gauche 40;
  draw_string "joueur : ";
  draw_string (string_of_int compteur.homme);
moveto gauche 10;
  draw_string "ordinateur : ";
  draw_string (string_of_int compteur.machine);
;;


(*renvoie la longueur du plus long mot de l (si mdt=0)*)
let rec max_length l mdt = (*max deje trouve*)
match l with
|[] -> mdt
|a::q -> max_length q (max (String.length a) mdt)
;;

let centre mot place =
let n = String.length mot in
let marge = (place-n) /2 in
String.concat "" [String.make marge ' '; mot; String.make (place - marge - n) ' ']
;;

open List

let liste_de_choix en_tete ll =
let l = ref (rev ll)
  and nb = length ll  and m = max_length ll 0
  and pos = ref 10 in
let box_w = m*char_w + 16  and box_h = char_h + 16 in
let decalage = box_h + 8 in
for i=1 to nb do
  let choix = hd !l in
  set_color cyan;
    fill_rect 10 !pos box_w box_h;
  set_color black;
      moveto 18 (8 + !pos); draw_string (centre choix m);
  l:= tl !l;
  pos:= !pos + decalage
done;
moveto 17 (10 + !pos); draw_string en_tete;
let status=wait_next_event [Button_down] in let (x,y) = (status.mouse_x,status.mouse_y) in
  let result = ref (-1) in
  if (10 <= x) & (x <= box_w) & (10 <= y) & (y <= 10 + !pos)
    then begin let i = (y-10) /decalage in result:= nb -i -1 end;
  set_color background;
    fill_rect 10 10 (max box_w (8 + (String.length en_tete)*char_w)) (!pos + char_h);
        (*pour effacer les boutons*)
!result;
;;

let confirmation () =
if (liste_de_choix "Etes-vous ser ?" ["Quitter !"; "En fait..."]) = 0
then exit 0;
;;

let rec changement () =
match liste_de_choix "Vous voulez..." ["changer le niveau"; "recommencer"; "rien"; "quitter"] with
|(-1) -> ()
|0 -> raise (Chgt "niveau")
|1 -> raise (Chgt "partie")
|2 -> () (*l'execution de la fonction qui a appele changement va reprendre*)
|3 -> confirmation (); changement ();
|_ -> failwith "changement"
;;

let rec qui_commence () =
match liste_de_choix "Qui commence ?" ["L'ordinateur"; "Le joueur"; "Quitter"] with
|(-1) -> qui_commence ()
|0 -> true
|1 -> false
|2 -> confirmation (); qui_commence ();
|_ -> failwith "qui_commence"
;;

let choisir_niveau () =
match liste_de_choix "Vous etes..." ["Debutant"; "Pas mauvais"; "Bon joueur"; "Joueur patient"] with
|(-1) -> raise (Chgt "niv pas choisi")
|0 -> 1
|1 -> 2
|2 -> 3
|3 -> 4
|_ -> failwith "choisir_niveau"
;;


(*fin de l'interface graphique, on passe au choses serieuses*)
(******************************************************************)

let copy_position p =
let plat = Array.make 7 [||] in
for i=0 to 6 do
  plat.(i) <- Array.copy p.plateau.(i)
done;
{plateau=plat; colonnes=(Array.copy p.colonnes)}
;;

let joue_coup coup joueur position =
let c=case_of_joueur joueur
  and p=copy_position position in
let n = (p.colonnes.(coup)).nbpp +1 in
let b = n=6 in (*colonne pleine apres avoir joue le coup ?*)
let ncol = {plein=b; nbpp=n} in (*la nouvelle colonne*)
p.colonnes.(coup) <- ncol;
p.plateau.(coup).(n-1) <- c;
p
;;

let coups_possibles position =
let l = ref [] and v=position.colonnes in
for i=0 to 6 do
  if not v.(i).plein then l:= i :: !l
done;
if !l = [] then raise (Cnv "plateau plein");
!l
;;

let joue_coup_homme position =
let cp = coups_possibles position
  and coup = ref (-1) in
while not (mem !coup cp) do
  try coup:= get_coup_homme () with
    |Cnv "Cliquer sur une colonne !" -> (moveto gauche (bas + 370); set_color black;
       draw_string "Cliquer sur une colonne !")
    |Chgt "" -> changement ();
done;
let result = joue_coup !coup Homme position in
draw_case Jaune (!coup, result.colonnes.(!coup).nbpp -1);
result;
;;

(********************************************************)
(*et ce qu'il manque pour que l'ordinateur joue :*)

let estime_graph =
let c bool a b c d e f = (*constructeur d'un noeud, ie de trois cas*)
    [|{configuration=a; joueur=bool; suivant=b; gagne=false};
      {configuration=c; joueur=bool; suivant=d; gagne=false};
      {configuration=e; joueur=bool; suivant=f; gagne=false};
    |]
  and t=true and f=false in
[| (*chaque element designe l'action e faire en fonction du pion suivant
     dans l'ordre Rouge, Trou, Jaune. Se referer au graph papier pour mieux voir !*)
(*0*) c  t 0 3  0 4  0 2 ;
(*1*) c  t 0 5  0 6  0 7 ;(*t est discutable*)
(*2*) c  f 0 0  0 8  0 9 ;
(*3*) c  t 0 10 0 11 0 2 ;
(*4*) c  t 0 12 0 13 0 7 ;
(*5*) c  t 0 14 0 15 0 2 ;
(*6*) c  t 0 16 0 17 0 18;(*t est discutable*)
(*7*) c  f 0 0  0 19 0 20;
(*8*) c  f 0 5  0 21 0 22;
(*9*) c  f 0 0  0 23 0 24;
(*10*) [|{configuration = -1; joueur = true; suivant = -1; gagne = true}; (*youpi!*)
         {configuration =  1; joueur = true; suivant = 11; gagne = false};
         {configuration =  0; joueur = true; suivant = 2 ; gagne = false}|]
(*enfin de l'action ! Les deux premiers indices sont hors-limites car ils ne doivent pas etre utilises : On a GAGNe !
*);
(*11*) c  t 2 12 3 13 0 7 ;
(*12*) c  t 2 14 4 15 0 2 ;
(*13*) c  t 5 16 6 17 0 18;
(*14*) c  t 1 10 7 11 0 2 ;
(*15*) c  t 4 12 8 13 0 7 ;
(*16*) c  t 3 14 8 15 0 2 ;
(*17*) [|{configuration = 6; joueur = true; suivant = 16; gagne = false};
         {configuration = 0; joueur = true; suivant = 17; gagne = false};
           (*joueur=true est discutable*)
         {configuration = 6; joueur = false; suivant = 18; gagne = false}|];
(*18*) c  f 0 0  8 19 3 20;
(*19*) c  f 0 5  8 21 4 22;
(*20*) c  f 0 0  7 23 1 24;
(*21*) c  f 0 16 6 17 5 18;
(*22*) c  f 0 0  4 19 2 20;
(*23*) c  f 0 5  3 21 2 22;
(*24*) [|{configuration =  0; joueur = false; suivant = 0 ; gagne = false};
         {configuration =  1; joueur = false; suivant = 23; gagne = false};
         {configuration = -1; joueur = false; suivant = -1; gagne = true}; (*youpi!*)|]
(*Les deux derniers indices sont hors-limites car ils ne doivent pas etre utilises : On a GAGNe !
*)
|];;

let estime_ligne parametres plateau dx dy xmin ymin longueur result =
let x=ref xmin and y=ref ymin
  and noeud = ref ( int_of_case plateau.(xmin).(ymin) ) in
for i=(-1) to longueur do (*on doit boucler longueur+3-1 fois*)
  x:= !x+dx; y:= !y+dy;
  let case = plateau.(!x).(!y) in
  let cas = estime_graph.(!noeud).(int_of_case case) in
  if cas.gagne then raise Gagnant;
  noeud:= cas.suivant;
  result:= !result + (if cas.joueur then parametres.(cas.configuration)
                                    else ~- (parametres.(cas.configuration)));
done;
;;

let estime parametres plateau =
let result = ref 0 in
for i=0 to 5 do (*horizontal*)
  estime_ligne parametres plateau 1 0 0 i 4 result;
done;
for i=0 to 6 do (*vertical*)
  estime_ligne parametres plateau 0 1 i 0 3 result;
done;
(*diagonal montant*)
  estime_ligne parametres plateau 1 1 0 2 1 result;
  estime_ligne parametres plateau 1 1 0 1 2 result;
  estime_ligne parametres plateau 1 1 0 0 3 result;
  estime_ligne parametres plateau 1 1 1 0 3 result;
  estime_ligne parametres plateau 1 1 2 0 2 result;
  estime_ligne parametres plateau 1 1 3 0 1 result;
(*diagonal descendant*)
  estime_ligne parametres plateau 1 (-1) 0 3 1 result;
  estime_ligne parametres plateau 1 (-1) 0 4 2 result;
  estime_ligne parametres plateau 1 (-1) 0 5 3 result;
  estime_ligne parametres plateau 1 (-1) 1 5 3 result;
  estime_ligne parametres plateau 1 (-1) 2 5 2 result;
  estime_ligne parametres plateau 1 (-1) 3 5 1 result;
!result
;;
(*doit toujours renvoyer entre mine et maxe*)


let inv_joueur = function
|Machine -> Homme
|Homme -> Machine
;;

let rec get_coup_machine profondeur joueur position parametres =
let cp = ref (coups_possibles position) in
let compare = ( match joueur with Machine -> (>=) |Homme -> (<=) ) in
let meilleur,pire = ( match joueur with Machine -> maxe,mine; |Homme -> mine,maxe ) in
let best_coup = ref {bcoup=hd !cp; valeur=pire} in
     (*vire e la premiere comparaison*)
     (*e modifier s'il n'y a pas toujours un coup possible*)
let switch = profondeur>1 in
(try while !cp <> [] do
       let coup_test=hd !cp in
       cp:= tl !cp;
       let position_test = joue_coup coup_test joueur position in
       (try let value = ref (estime parametres position_test.plateau) in (*c'est ici que se souleve Gagnant*)
           if switch (*then creuser plus profond*) then value :=
             (get_coup_machine (profondeur-1) (inv_joueur joueur) position_test parametres).valeur;
           if compare !value !best_coup.valeur then best_coup:={bcoup=coup_test; valeur= !value};
         with Gagnant -> raise (Gagne coup_test););
       done;
       !best_coup;
with Gagne c -> {bcoup=c; valeur=meilleur});
;;

let joue_coup_machine niveau position parametres =
let coup_e_jouer = (get_coup_machine niveau Machine position parametres).bcoup in
let result = joue_coup coup_e_jouer Machine position in
draw_case Rouge (coup_e_jouer, result.colonnes.(coup_e_jouer).nbpp -1);
result;
;;


let test_gagne plateau =
try (let _=estime [|0;0;0; 0;0;0; 0;0;0|] plateau in false) with
  |Gagnant -> true
;;

(*enfin la conclusion*)
let main parametres =
clear_graph ();
let niveau = ref 2 in
let score = {homme=0; machine=0} in (*de type compteur*)
while true do (*une partie*)
  let temp = qui_commence () in
  let position_courante = ref
    {plateau = [|[|Trou; Trou; Trou; Trou; Trou; Trou; |];
                 [|Trou; Trou; Trou; Trou; Trou; Trou; |];
                 [|Trou; Trou; Trou; Trou; Trou; Trou; |];
                 [|Trou; Trou; Trou; Trou; Trou; Trou; |];
                 [|Trou; Trou; Trou; Trou; Trou; Trou; |];
                 [|Trou; Trou; Trou; Trou; Trou; Trou; |];
                 [|Trou; Trou; Trou; Trou; Trou; Trou; |];|];
     colonnes = [|{plein=false; nbpp=0};
                  {plein=false; nbpp=0};
                  {plein=false; nbpp=0};
                  {plein=false; nbpp=0};
                  {plein=false; nbpp=0};
                  {plein=false; nbpp=0};
                  {plein=false; nbpp=0};|]} in
  draw_plateau !position_courante.plateau;
  if temp then position_courante:= joue_coup_machine !niveau !position_courante parametres;
  let fini = ref false in
  while not !fini do (*un coup du joueur et de la machine*)
    let joue = ref false in
    while not !joue do (*un coup du joueur*)
      try (position_courante:= joue_coup_homme !position_courante; joue:= true) with
      |Chgt "niveau" -> (try niveau:= choisir_niveau() with Chgt "niv pas choisi" -> ());
      |Chgt "partie" -> joue:= true; fini:= true;
                        score.machine <- score.machine +1;
      |Cnv "plateau plein" -> joue:= true; fini:= true;
    done;
    if test_gagne !position_courante.plateau
    then (fini:= true; score.homme <- score.homme +1;)
    else begin
      (try position_courante:= joue_coup_machine !niveau !position_courante parametres; with
         |Cnv "plateau plein" -> fini:= true;);
      if test_gagne !position_courante.plateau
      then (fini:= true; score.machine <- score.machine +1)
    end;
  done;
    print_score score;
done;;

main [|0; 5835; 100000; 413; 7021; 2241; 2412; 78592; 8977|];;
