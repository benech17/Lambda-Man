(**

   Ce module fournit un générateur aléatoire de mondes. Il est peu
   sophistiqué, c'est à vous de l'améliorer pour briller dans la tâche
   5 du projet.

*)

open World


let default_end_of_time =
  100000

let default_visibility =
  100.

let initial = {
    space       = Space.empty;
    trees       = [];
    teams       = [];
    epoch       = 0;
    microcodes  = [];
    end_of_time = default_end_of_time;
    visibility  = default_visibility;
}

let simple_world nb_players nb_robots_per_team max_hell max_ground max_tree =
  let width  = Ext.Float.random_in_range 500. 1000. in
  let height = Ext.Float.random_in_range 500. 1000. in
  let random_angle () = Space.angle_of_float (Random.float (2. *. Float.pi)) in
  let random_position space =
    let rec aux n =
      if n = 0 then failwith "Impossible to find a free random position.";
      let p = Ext.Float.(
        (random_in_range 0. width, random_in_range 0. height)
      ) in
      if Space.inside p (fun _ -> true) space <> None then aux (n - 1) else p
    in
    aux 10
  in
  let random_size () =
    Ext.Float.random_in_range 100. (width /. 10.)
  in
  let rec make_hell space =
    let s = Space.square (random_position space) (random_size ()) Hell in
    if Space.polygon_overlaps s space (( = ) Hell) then make_hell space else
    Space.(blend space (polygon s))
  in
  let make_ground space =
    let ratio = Ground (Ext.Float.random_in_range 0.5 1.5) in
    let s = Space.square (random_position space) (random_size ()) ratio in
    Space.(blend space (polygon s))
  in
  let make_tree space _ =
    let tree_position = random_position space in
    let branches = Ext.Int.random_in_range 2 20 in
    { tree_position; branches } in

  let make_team pos space team_identifier =
    let spaceship = match pos with | Some(p) -> p | None -> random_position space in
    let make_robot id =
      make_robot id team_identifier spaceship (random_angle ())
    in
    { team_identifier; spaceship;
      robots = Ext.Fun.repeat nb_robots_per_team make_robot }
  in



  let create_cross pos = 
    let a1 = Space.rectangle (pos) (600.) (40.) Hell and a2 = Space.rectangle (pos) (40.) (600.) Hell in [a1;a2]
    
  in

  let x_ = fst and y_ = snd in

  let create_lab pos = 
    let s1 = Space.rectangle (pos) (900.) (15.) Hell and s2 = Space.rectangle ((x_ pos , y_ pos +. 200.)) (900.) (15.) Hell
    and s3 = Space.rectangle ((x_ pos -. 450., y_ pos +. 80. )) (15.) (170.) Hell
    and s4 = Space.rectangle ((x_ pos -. 400., y_ pos +. 120.)) (15.) (170.) Hell
    and s5 = Space.rectangle ((x_ pos -. 350., y_ pos +. 80.)) (15.) (170.) Hell
    and s6 = Space.rectangle ((x_ pos -. 300., y_ pos +. 120.)) (15.) (170.) Hell
    and s7 = Space.rectangle ((x_ pos -. 250., y_ pos +. 80.)) (15.) (170.) Hell
    and s8 = Space.rectangle ((x_ pos -. 200., y_ pos +. 120.)) (15.) (170.) Hell
    and s9 = Space.rectangle ((x_ pos -. 150., y_ pos +. 80.)) (15.) (170.) Hell
    and s10 = Space.rectangle ((x_ pos -. 100., y_ pos +. 120.)) (15.) (170.) Hell
    and s11 = Space.rectangle ((x_ pos -. 50., y_ pos +. 80.)) (15.) (170.) Hell
    and s12 = Space.rectangle ((x_ pos , y_ pos +. 120.)) (15.) (170.) Hell
    and s13 = Space.rectangle ((x_ pos +. 50. , y_ pos +. 80.)) (15.) (170.) Hell

    and s14 = Space.rectangle ((x_ pos +. 150. , y_ pos +. 120.)) (7.) (180.) Hell
    and s15 = Space.rectangle ((x_ pos +. 170., y_ pos +. 80.)) (7.) (180.) Hell 
    and s16 = Space.rectangle ((x_ pos +. 190., y_ pos +. 120.)) (7.) (180.) Hell

    and s17 = Space.rectangle ((x_ pos +. 450., y_ pos +. 80.)) (15.) (170.) Hell
    and s18 = Space.rectangle ((x_ pos +. 400. , y_ pos -. 80.)) (15.) (170.) Hell

    in [s1;s2;s3;s4;s5;s6;s7;s8;s9;s10;s11;s12;s13;s14;s15;s16;s17;s18] in

  let create_hell pos= 
    let rec auxc l space = match l with 
    |[]-> space 
    |x::xs-> auxc xs Space.(blend space (polygon x)) in

     auxc (create_lab pos) Space.empty 
  in

  let create_single_tree pos = 
    let branches = Ext.Int.random_in_range 2 20 and tree_position =pos in
      { tree_position ; branches}

  in


  let nb_hell = Ext.Int.random_in_range 1 max_hell in

  let random_pos = random_position Space.empty in
  let nb_hell = Ext.Int.random_in_range 1 max_hell in
  let space = create_hell random_pos in
    (* let space = Ext.Fun.iter nb_hell make_hell space in   *)
  let nb_grounds = Ext.Int.random_in_range 1 max_ground in
  let space = Ext.Fun.iter nb_grounds make_ground space in
  let nb_trees = Ext.Int.random_in_range 1 max_tree in
  (* let trees = Ext.Fun.repeat nb_trees (make_tree space) in *)
  let trees = [create_single_tree (x_ random_pos +. 550. , y_ random_pos +. 50.)] in
  let spos =  (x_ random_pos -. 550. , y_ random_pos +. 100.) in
  let teams = Ext.Fun.repeat nb_players (make_team (Some(spos)) space) in
  { initial with space; trees; teams }

let output world =
  to_yojson world |> Yojson.Safe.pretty_to_string |> output_string stdout



let generate
      visualize nb_players nb_robots_per_teams
      max_hell max_ground max_tree =
  let world =
    simple_world nb_players nb_robots_per_teams max_hell max_ground max_tree
  in
  if visualize then (Visualizer.(show world; pause ()));
  output world

let visualization_flag = Cmdliner.(Arg.(
  value & flag & info ["v"]
  ~doc:"Visualize the generated world")
)

let nb_players = Cmdliner.(Arg.(
  value & opt int 1 & info ["p"]
  ~docv:"NBPLAYERS"
  ~doc:"Handle $(docv) players."
))

let nb_robots = Cmdliner.(Arg.(
  value & opt int 1 & info ["r"]
  ~docv:"NBROBOTS"
  ~doc:"Handle $(docv) robots per player."
))

let max_hell = Cmdliner.(Arg.(
  value & opt int 1 & info ["h"]
  ~docv:"MAXHELL"
  ~doc:"Use a maximum of $(docv) hell blocks."
))

let max_ground = Cmdliner.(Arg.(
  value & opt int 1 & info ["g"]
  ~docv:"MAXGROUND"
  ~doc:"Use a maximum of $(docv) ground blocks."
))

let max_tree = Cmdliner.(Arg.(
  value & opt int 1 & info ["t"]
  ~docv:"MAXTREE"
  ~doc:"Use a maximum of $(docv) trees."
))

let cmd = Cmdliner.(
  let doc   = "Generate a random world." in
  let exits = Term.default_exits in
  Term.(const generate $ visualization_flag
        $ nb_players $ nb_robots
        $ max_hell $ max_ground $ max_tree),
  Term.info "generate" ~doc ~exits
)
