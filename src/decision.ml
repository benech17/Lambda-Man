(**

   Chers programmeuses et programmeurs de λman, votre mission consiste
   à compléter ce module pour faire de vos λmen les meilleurs robots
   de la galaxie. C'est d'ailleurs le seul module qui vous pouvez
   modifier pour mener à bien votre mission.

   La fonction à programmer est

         [decide : memory -> observation -> action * memory]

   Elle est appelée à chaque unité de temps par le Λserver avec de
   nouvelles observations sur son environnement. En réponse à ces
   observations, cette fonction décide quelle action doit effectuer le
   robot.

   L'état du robot est représenté par une valeur de type [memory].  La
   fonction [decide] l'attend en argument et renvoie une nouvelle
   version de cette mémoire. Cette nouvelle version sera passée en
   argument à [decide] lors du prochain appel.

*)

open World
open Space

(** Le Λserver transmet les observations suivantes au λman: *)
type observation = World.observation

(** Votre λman peut se déplacer : il a une direction D et une vitesse V.

    La direction est en radian dans le repère trigonométrique standard :

    - si D = 0. alors le robot pointe vers l'est.
    - si D = Float.pi /. 2.  alors le robot pointe vers le nord.
    - si D = Float.pi alors le robot pointe vers l'ouest.
    - si D = 3 * Float.pi / 2 alors le robot pointe vers le sud.
      (Bien entendu, ces égalités sont à lire "modulo 2 * Float.pi".)

    Son déplacement en abscisse est donc V * cos D * dt et en ordonnée
    V * sin D * dt.

    Votre λman peut communiquer : il peut laisser du microcode sur sa
    position courante pour que d'autres λmen puissent le lire.  Un
    microcode s'autodétruit au bout d'un certain nombre d'unités de
    temps mais si un microcode est laissé près d'un autre microcode
    identique, ils fusionnent en un unique microcode dont la durée de
    vie est le somme des durées de vie des deux microcodes initiaux.
    Construire un microcode demande de l'énergie au robot : chaque
    atome lui coûte 1 point d'énergie. Heureusement, l'énergie augmente
    d'1 point toutes les unités de temps.

    Pour terminer, votre λman peut couper des arbres de Böhm. Les
    arbres de Böhm ont un nombre de branches variables. Couper une
    branche prend une unité de temps et augmente le score de 1
    point. Si on ramène cette branche au vaisseau, un second point est
    accordé.

    Pour finir, le monde est malheureusement très dangereux : on y
    trouve des bouches de l'enfer dans lesquelles il ne faut pas tomber
    ainsi que des champs de souffrances où la vitesse de votre robot
    est modifiée (de -50% à +50%).

*)

type action =
  | Move of Space.angle * Space.speed
  (** [Move (a, v)] est l'angle et la vitesse souhaités pour la
      prochaine unité de temps. La vitesse ne peut pas être négative et
      elle ne peut excéder la vitesse communiquée par le serveur. *)

  | Put of microcode * Space.duration
  (** [Put (microcode, duration)] pose un [microcode] à la position courante
      du robot. Ce microcode s'autodétruira au bout de [duration] unité de
      temps. Par contre, s'il se trouve à une distance inférieure à
      [Space.small_distance] d'un microcode similaire, il est fusionné
      avec ce dernier et la durée de vie du microcode résultant est
      la somme des durées de vide des deux microcodes. *)

  | ChopTree
  (** [ChopTree] coupe une branche d'un arbre de Böhm situé une distance
      inférieure à [Space.small_distance] du robot. Cela augmente le score
      de 1 point. *)

  | Wait
  (** [Wait] ne change rien jusqu'au prochain appel. *)

  | Die of string
  (** [Die] est produit par les robots dont on a perdu le signal. *)

[@@deriving yojson]

(**

   Le problème principal de ce projet est le calcul de chemin.

   On se dote donc d'un type pour décrire un chemin : c'est une
   liste de positions dont la première est la source du chemin
   et la dernière est sa cible.

*)
type path = Space.position list

(** Version lisible des chemins. *)
let string_of_path path =
  String.concat " " (List.map string_of_position path)

(**

   Nous vous proposons de structurer le comportement du robot
   à l'aide d'objectifs décrits par le type suivant :

*)
type objective =
  | Initializing            (** Le robot doit s'initialiser.       *)
  | Chopping                (** Le robot doit couper des branches. *)
  | GoingTo of path * path
  (** Le robot suit un chemin. Le premier chemin est la liste des
      positions restantes tandis que le second est le chemin initial.
      On a donc que le premier chemin est un suffixe du second. *)

(** Version affichable des objectifs. *)
let string_of_objective = function
  | Initializing -> "initializing"
  | Chopping -> "chopping"
  | GoingTo (path, _) ->
    Printf.sprintf
      "going to %s" (String.concat " " (List.map string_of_position path))

(**

   Comme dit en introduction, le robot a une mémoire qui lui permet de
   stocker des informations sur le monde et ses actions courantes.

   On vous propose de structurer la mémoire comme suit:

*)
type memory = {
  known_world : World.t option;      (** Le monde connu par le robot.     *)
  graph       : Graph.t;             (** Un graphe qui sert de carte.     *)
  objective   : objective;           (** L'objectif courant du robot.     *)
  targets     : Space.position list; (** Les points où il doit se rendre. *)
  id : int;                          (** L'identifiant du robot           *)
  is_init : bool;                    (** Si il est initaliser ou pas      *)
}

(**

   Initialement, le robot ne sait rien sur le monde, n'a aucune cible
   et doit s'initialiser.

*)
let initial_memory = {
  known_world = None;
  graph       = Graph.empty;
  objective   = Initializing;
  targets     = [];
  id = 0;
  is_init = false;
}

(**

   Traditionnellement, la fonction de prise de décision d'un robot
   est la composée de trois fonctions :

   1. "discover" qui agrège les observations avec les observations
      déjà faites dans le passé.

   2. "plan" qui met à jour le plan courant du robot en réaction
      aux nouvelles observations.

   3. "next_action" qui décide qu'elle est l'action à effectuer
       immédiatement pour suivre le plan.

*)

(** [discover] met à jour [memory] en prenant en compte les nouvelles
    observations. *)
let discover visualize graphic observation memory =
  let seen_world = World.world_of_observation observation in
  let known_world =
    match memory.known_world with
    | None -> seen_world
    | Some known_world -> World.extends_world known_world seen_world
  in
  if visualize then Visualizer.show ~force:true known_world;
  if graphic then VisualizerGraphic.show ~force:true known_world;
  { memory with known_world = Some known_world }


(*

   Pour naviguer dans le monde, le robot construit une carte sous la
   forme d'un graphe.

   Les noeuds de ce graphe sont des positions clées
   du monde. Pour commencer, vous pouvez y mettre les sommets des
   polygones de l'enfer, le vaisseau, le robot et les arbres.

   Deux noeuds sont reliés par une arête si le segment dont ils
   sont les extremités ne croisent pas une bouche de l'enfer.

*)

let distance a b =  match dist2 a b with |Distance(f) -> f 

let get_hell_poly memory observation =  let kw_space = match memory.known_world with | Some(kw) -> kw.space | _ -> failwith("bad discovery") in
  polygons  kw_space (fun p-> p = Hell)

let get_ground_poly memory observation =  let kw_space = match memory.known_world with | Some(kw) -> kw.space | _ -> failwith("bad discovery") in
  polygons  kw_space (fun p-> p != Hell)

let hell_poly_list memory observation = List.map bounding_box_of_positions 
    ((List.map Space.vertices (get_hell_poly memory observation) ))

let ground_poly_list memory observation  = List.map bounding_box_of_positions 
    ((List.map Space.vertices (get_ground_poly memory observation) ))

let bounding_box_to_list lb = let b_to_list b = match b with
    | ((x,y) , (x',y')) ->  let x=(min x x') -. 1. and x'=(max x x') +. 1. and y=(min y y') -. 1. and y'=(max y y') +. 1.
      in [(x,y);(x',y');(x,y');(x',y)] in
  List.map b_to_list lb     

let get_hell_nodes memory observation = 
  List.flatten (bounding_box_to_list (hell_poly_list memory observation))

let get_ground_list memory observation = 
  List.flatten (bounding_box_to_list (ground_poly_list memory observation))


let get_nodes_list memory observation =

  let kw_space = match memory.known_world with | Some(kw) -> kw.space | _ -> failwith("bad discovery") in
  let pl = polygons  kw_space (fun p-> true) in
  let l = 
  ( World.tree_positions observation.trees)@(get_hell_nodes memory observation ) @
  (get_ground_list memory observation) @[observation.spaceship]@ [observation.position] in l

  (* (List.filter (fun n -> not (List.exists (inside_polygon n) pl) ) l)@ [observation.position] *)


(* renvoie les aret du graphe complet *)
let get_edges_list memory observation  =  
  let get_couples l = let aux l x = List.map (fun e->(e,x)) l in
    List.flatten (List.map (fun e-> aux l e) l) in
  get_couples (get_nodes_list memory observation)

(* a est un segment et l une liste de segments , si a insersete avec un element de l renvoi true*)
let has_intersection l a = List.exists (Space.segment_intersects a) l

(* tout les segment qui s'interscte pas dans les deux liste s1 , s2 *)

let hell_segments memory observation =   
  match memory.known_world with
  | None -> failwith("bad discovery")
  | Some kw -> World.hell_segments kw 



(* renvoi le pos d'intersection *)
let intersection_points seg1 seg2 =
  match seg1 , seg2 with
  | ((ax,ay) , (bx,by)) , ((cx,cy) , (dx,dy)) -> 
    let r =  ( ((ay -.cy )*.(dx-.cx)) -.( (ax-.cx)*. (dy-.cy)  ) ) /. ( ((bx-.ax)*.(dy-.cy)) -.( (by -. ay)*. (dx-.cx)  ) ) in
    ((ax +. (r *.(bx-.ax))),(ay+.(r *. (by -. ay))))



let rec list_uniq l = match l with 
  | e :: l' -> if List.mem e l' then list_uniq l' else e::list_uniq l'
  | _ -> []


let point_intersect seg poly = 
  (* la liste de tout les points d'intesect de seg avec poly_seg *)
  let rec aux seg poly_seg =  match poly_seg  with 
    | [] -> []
    | e::l' ->  if (Space.segment_intersects seg e) then  (intersection_points seg e)::(aux seg l' ) else (aux seg l' ) in
  list_uniq (aux seg (Space.polygon_segments poly))


(*renvoie tout les aretes qui passsent pas par l'enfer *)
let filtered_edges memory observation =
  let no_passing_hell memory observation = let edges = get_edges_list memory observation and hell_seg = hell_segments memory observation in
    List.filter (fun e-> not (has_intersection hell_seg e)) edges in

  List.map (fun x-> match x with |(a,b)-> (a,b,distance a b)) (no_passing_hell memory observation) 



let visibility_graph observation memory =

  Graph.make (get_nodes_list memory observation) (filtered_edges memory observation)

(* Students, this is your job! *)


(*

   Il nous suffit maintenant de trouver le chemin le plus rapide pour
   aller d'une source à une cible dans le graphe.

*)

module Node = 
struct
  type t = Graph.node 

  let compare a b =
    match (a,b) with 
    | (x1,x2) , (y1,y2) -> if x1 < y1 then -1
      else if x1 > y1 then 1
      else if x2 < y2 then -1
      else if x2 > y2 then 1
      else 0

end;;

module File =PriorityQueue.Make (Node) (Float)

module Listasso = Map.Make(Node) 

(* met la valeur de tout les cle neoud a false visited: map*)
let rec init_visited visited l=match l with
  |[]->visited
  |x::xs-> init_visited (Listasso.add x false visited) xs

(* met les priorite de tout les noeud=source a 0 sinon infinity*)
let rec init_file f source l=match l with
  |[]->f
  |x::xs->if(x=source) then init_file (File.insert f x 0.) source xs 
    else init_file (File.insert f x infinity) source xs

(* initalise le pred de tout x a x*)
let rec init_pred visited l=match l with
  |[]->visited
  |x::xs-> init_pred (Listasso.add x x visited) xs

(* lvisied : map ,  actualise la distance de tout les sommmet adjacent a un sommet de priorite prio*)
let rec dij_aux prio ledge lpere fi lvisited=match ledge with
  |[]->(fi,lpere)
  |(a,b,d)::xs-> 
    if not(Listasso.find b lvisited) && (prio +. d < (File.priority fi b) ) then 
      let nouvpere=Listasso.add b a lpere in  
      let nouvfi=File.decrease fi b (prio +. d) in 
      dij_aux prio xs nouvpere nouvfi lvisited
    else dij_aux prio xs lpere fi lvisited

let rec dijkstra prede file graph lpere_visited=match File.length file with
  |0->prede
  |_->let min_el=File.get_min file in 
    let new_file=File.remove_min file in
    match min_el with
    |None->prede
    |Some(prio,key)->let  nouvpere_visi =(Listasso.add key true lpere_visited) in
      let (fi,pr)= (dij_aux prio (Graph.out graph key) prede new_file nouvpere_visi) in 
      dijkstra pr fi graph nouvpere_visi

(*res: map , target : key , source:  renvoi une liste de noeud*)
let rec find_path res source target=
  match Listasso.find target res with
  | s when s = source -> [source]@[target]
  | p ->(find_path res source p)@[target]

let shortest_path graph source target : path =
  let file=init_file File.empty source (Graph.nodes graph) in 
  let prede_visted=init_visited Listasso.empty (Graph.nodes graph) in
  let prede=init_pred Listasso.empty (Graph.nodes graph) in 
  let res_jik= (dijkstra prede file graph prede_visted)in 
  find_path res_jik source target 




(* *)
let calc_weight a b p1 p2 w suf dis =  
  if (distance a p1) < (distance a p2) then 
    let dis = (distance a p1) +. (dis /. suf) +. (distance p2 b) in
    max dis w
  else 
    let dis = (distance a p2) +. (dis /. suf) +. (distance p1 b) in
    max dis w

(* calcule le centre de (a,b)*)
let calc_center a b =
  match (a,b) with 
  | ((x1,y1) , (x2,y2)) -> (((x1 +. x2) /. 2.) , ((y1 +. y2) /. 2.) )  

(* ground poly: liste de polygon - *)
(*intersectionPoints: l'intersection de (a,b) avec tout les segment d'un polygon de la liste groundPoly*)
let rec update_weight a b w  groundPoly observation world =
  match groundPoly with 
  | [] -> (a,b, w)
  | x::xs -> 
    let intersectionPoints = (point_intersect (a,b) x ) in
    if ( List.length intersectionPoints = 2 ) then
      let point1 = List.nth intersectionPoints 0 in
      let point2 = List.nth intersectionPoints 1 in
      let milieu = calc_center point1 point2 in  
      let suffering = World.suffering world milieu in 
      let new_weight = calc_weight a b point1 point2 w suffering (distance point1 point2) in
      update_weight a b new_weight  xs observation world
    else update_weight a b w  xs observation world

(* actualise la liste des sommmet en prenant en compte les champs de soufrence*)
let rec new_edges edgesList groundPoly ground_seg observation world =  
  match edgesList with 
  | [] -> [] 
  | (a,b,w)::xs -> 
    if (has_intersection  ground_seg (a,b)) then (update_weight a b w  groundPoly observation world)::new_edges xs groundPoly ground_seg observation world
    else (a,b,w)::new_edges xs groundPoly ground_seg observation world


let modify_graph g groundPoly ground_seg observation known_world = 
  let nodeList = Graph.nodes g in 
  let edgesList = Graph.edges g in 
  let newEdgeList = new_edges edgesList groundPoly ground_seg observation known_world in 
  Graph.make nodeList newEdgeList    

let update_path source target g groundPoly ground_seg observation known_world old_path =
  let graph = modify_graph g groundPoly ground_seg observation known_world in
  let new_path = shortest_path graph source target in
  if List.length old_path > 1 then new_path@(List.tl old_path) else new_path
(**

   [plan] doit mettre à jour la mémoire en fonction de l'objectif
   courant du robot.

   Si le robot est en tr ain de récolter du bois, il n'y a rien à faire
   qu'attendre qu'il est fini.

   Si le robot est en train de suivre un chemin, il faut vérifier que
   ce chemin est encore valide compte tenu des nouvelles observations
   faites, et le recalculer si jamais ce n'est pas le cas.

   Si le robot est en phase d'initialisation, il faut fixer ses cibles
   et le faire suivre un premier chemin.

*)


let rec ground_segments polyList =
  match polyList with 
  | [] -> []
  | x::xs -> (Space.polygon_segments x)@ground_segments xs

let plan visualize graphic observation memory =
  let graphe = visibility_graph observation memory in
  match memory.objective with
  | Initializing ->  let targets' = ( World.tree_positions observation.trees) in
        {memory with graph = graphe; targets= targets'}

  | Chopping -> memory

  | GoingTo (p1,p2) -> 
      let kw =match memory.known_world with |Some(w)-> w |None -> failwith("bad discovery") and
      ground_poly = polygons observation.around (fun p-> p!=Hell)  in
      let ground_seg = ground_segments ground_poly and
        hell_seg = hell_segments memory observation in
      let has_hell_inter = has_intersection hell_seg (observation.position,List.hd p1)  and 
        has_ground_inter = has_intersection ground_seg (observation.position,List.hd p1)  in 
      let new_path = match (has_hell_inter, has_ground_inter) with 
        | (true,_) -> shortest_path graphe (observation.position) (List.hd p1)
        | (_,true) -> update_path (observation.position) (List.hd p1) graphe ground_poly ground_seg observation kw p1
        | (_,_) -> p1 in
      let objective' = GoingTo(new_path, p2) in
      { memory with objective = objective' ; graph = graphe}


(**

   Next action doit choisir quelle action effectuer immédiatement en
   fonction de l'objectif courant.

   Si l'objectif est de s'initialiser, la plannification a mal fait
   son travail car c'est son rôle d'initialiser le robot et de lui
   donner un nouvel objectif.

   Si l'objectif est de couper du bois, coupons du bois! Si on vient
   de couper la dernière branche, alors il faut changer d'objectif
   pour se déplacer vers une autre cible.

   Si l'objectif est de suivre un chemin, il faut s'assurer que
   la vitesse et la direction du robot sont correctes.

*)

let rec micro_index micro_list id acc = match micro_list with
  | [] -> failwith("index of id not found in micro_list")
  | x :: xs -> match x.microcode with 
      | MicroAtom(i) -> if i=id then acc else micro_index xs id (acc+1)
      | _ -> micro_index xs id (acc+1)

let compute_angle p1 p2 = 
  let x1 = Space.x_ p1 and
  x2 = Space.x_ p2 and
  y1 = Space.y_ p1 and
  y2 = Space.y_ p2 in
  Space.angle_of_float (Float.atan2 (y2 -. y1)  (x2 -. x1))



let rec sublist b e l = 
  match l with
    [] -> failwith "sublist"
  | h :: t -> 
     let tail = if e=0 then [] else sublist (b-1) (e-1) t in
     if b>0 then tail else h :: tail


let split_targets observation memory n i = let t = memory.targets in let nb_t = List.length t and
   pos = observation.position in
   let cut = nb_t / n and rest_cut = nb_t mod n in
   let targets' =if i !=0 then sublist (i*cut) ((i+1)*(cut) -1) t else
   (sublist (i*cut) ((i+1)*(cut) -1) t) @ (sublist (nb_t - rest_cut) (nb_t -1) t) in
   let objective' = GoingTo([List.hd targets'],[observation.position]) in
   Move (compute_angle pos (List.hd targets'), observation.max_speed) , {memory with objective = objective'; targets=targets'}
  

let next_action visualize graphic observation memory =
  let pos = observation.position in
  match memory.objective with
  
  | Initializing -> let id = memory.id and micro_list = observation.messages in

      let nb_micro = List.length micro_list in
      if nb_micro != 0 then
        let index = micro_index micro_list id 0 in
        if memory.is_init then
          split_targets observation memory nb_micro index
        else Move (observation.angle, Space.speed_of_float 0.) , memory
      else
        Put( MicroAtom id, duration_of_int 100 ) , {memory with is_init=true}

  | Chopping ->  let tree= match World.tree_at observation.trees pos with
        | None -> failwith("Chopping null tree!")
        | Some(t) -> t
      and targets' = List.tl memory.targets
      in let tpos = if targets'=[] then observation.spaceship else List.hd targets' in
      if(tree.branches <= 0) then let objective' =  GoingTo ([tpos], [tpos]) in
        (Wait , {memory with objective = objective' ; targets = targets'}) 
      else (ChopTree , memory) 

  | GoingTo (p1,p2) -> let pos = observation.position and t_pos = List.hd p1 and branche_nb = match tree_at observation.trees pos with
      | None -> 0 | Some(n) -> n.branches 
      and new_path = if List.length p1 = 1 then p1 else List.tl p1 in
      
      if(t_pos = observation.spaceship && Space.close t_pos pos 1.) then Move (observation.angle, Space.speed_of_float 0.), memory
      else if(branche_nb > 0) then Move (observation.angle, Space.speed_of_float 0.), {memory with objective=Chopping} else
      if Space.close t_pos pos 1. then Move (compute_angle pos (List.hd new_path) , observation.max_speed) , {memory with objective=GoingTo( new_path , [pos]) }
      else Move (compute_angle pos t_pos, observation.max_speed), memory 


(**

   Comme promis, la fonction de décision est la composition
   des trois fonctions du dessus.

*)

(*visualize pour GUI simple et graphic pour GUI complexe*)

let decide visualize graphic observation memory : action * memory =
  let memory = discover visualize graphic observation memory in
  let memory = plan visualize graphic observation memory in
  
  let () =if visualize then Visualizer.show_graph memory.graph else 
    VisualizerGraphic.show_graph memory.graph in
  next_action visualize graphic observation memory