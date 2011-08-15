(* Some config stuff *)
let container_top = 10;;
let container_left = 10;;
let container_height = 300;;
let container_width = 700;;

let bar_height = 100;;
let bar_width = 5;;
let ball_size = 5;;

let left_bar_X = 10;;
let right_bar_X = container_width - 10 - bar_width;;
let left_bar_init_Y = 50;;
let right_bar_init_Y = 50;;

let minX = left_bar_X + bar_width;;
let maxX = right_bar_X - ball_size;;
let minY = 0;;
let maxY = container_height - (2 * ball_size);;

(* The type representation of the ball *)
type direction = TopLeft | TopRight | BottomLeft | BottomRight

let initBallX = 250;;
let initBallY = 50;;
let initBallDirection = BottomRight;;
let ballMvt = 5;;
let ballUpdateInterval = 100.0;;

let barMvt = 5;;

(* Some "primitives" we will use to access the DOM tree *)
let move_elt elt top left =
        elt#_get_style#_set_marginTop ((string_of_int top) ^ "px");
        elt#_get_style#_set_marginLeft ((string_of_int left) ^ "px");;

let create_rect (doc : Dom.document) id height width =
        let rect = doc#createElement "div" in
        rect#setAttribute "id" id;
        rect#_get_style#_set_position "absolute";
        rect#_get_style#_set_height ((string_of_int height) ^ "px");
        rect#_get_style#_set_width ((string_of_int width) ^ "px");
        rect#_get_style#_set_border "solid 1px black";
        rect;;

let create_bar doc id =
        create_rect doc id bar_height bar_width;;

let getRightbarCoords _ =
        let rbar = Dom.document#getElementById "right_bar" in
        let yh = int_of_string rbar#_get_style#_get_marginTop in
        (yh, yh + bar_height)

let create_ball doc id =
        let ball = create_rect doc id ball_size ball_size in
        ball

(* The reactions of the ball *)
let rec getNextBallPos currentX currentY direction =
        let fup = getNextBallPos currentX currentY in

        (* If we are in a corner, there in only one possible direction to get out.
         * Choose it right away (simplifies the rest of the code) *)

        if currentX <= minX && currentY <= minY && direction != BottomRight then fup BottomRight
        else if currentX <= minX && currentY >= maxY && direction != TopRight then fup TopRight
        else if currentX >= maxX && currentY <= minY && direction != BottomLeft then fup BottomLeft
        else if currentX >= maxX && currentY >= maxY && direction != TopLeft then fup TopLeft
        else

        match direction with
        | TopLeft -> 
                        if currentX <= minX then
                                fup TopRight
                        else
                                if currentY <= minY then
                                        fup BottomLeft
                                else
                                       (currentX - ballMvt, currentY - ballMvt, direction) 
        | TopRight -> 
                        if currentY <= minY then
                                fup BottomRight
                        else
                                if currentX >= maxX then
                                        fup TopLeft
                                else
                                        (currentX + ballMvt, currentY - ballMvt, direction)
        | BottomLeft ->
                        if currentX <= minX then
                                fup BottomRight
                        else
                                if currentY >= maxY then
                                        fup TopLeft
                                else
                                        (currentX - ballMvt, currentY + ballMvt, direction)
        | BottomRight ->
                        if currentY >= maxY then
                                fup TopRight
                        else
                                if currentX >= maxX then
                                        fup BottomLeft
                                else
                                        (currentX + ballMvt, currentY + ballMvt, direction)

let advance_ball (ball:Dom.element) ((oldX, oldY, oldDir):int * int * direction) : (int * int * direction) =
        let (newX, newY, newDir) = getNextBallPos oldX oldY oldDir in
        move_elt ball newY newX;
        (newX, newY, newDir);; 

let isLosingPosition (x, y, dir) : bool = 
        let (ymin,ymax) = getRightbarCoords () in
        if x >= maxX && (y < ymin || y > ymax) then
                true
        else
                false;;

let rec step (coords:int * int * direction) _ = 
        let newCoords = advance_ball (Dom.document#getElementById "ball") coords in
        if isLosingPosition newCoords then
                Dom.window#alert "You suck. Seriously."
        else
                ignore (Dom.window#setTimeout (step newCoords) ballUpdateInterval);;
        
(* Keyboard controls *)
(* keycode 38 : up
 * keycode 40 : down *)
let move_player_bar y =
        let bar = Dom.document#getElementById "right_bar" in
        let oldY = int_of_string bar#_get_style#_get_marginTop in
        let newY = max 0 (min (oldY + y) (container_height - bar_height)) in
        bar#_get_style#_set_marginTop (string_of_int newY ^ "px");;

let mainDivKeyProcess (key:Dom.keyEvent) : bool =
        let keycode = key#_get_keyCode in
        if keycode = 38 then
                (move_player_bar (- barMvt); true)
        else if keycode = 40 then
                (move_player_bar (+ barMvt); true)
        else false

(* The global boot function *)                
let onload _ = 
        let doc = Dom.document in
        let main_div = doc#getElementById "main" in
        main_div#_get_style#_set_position "absolute";
        main_div#_get_style#_set_marginTop (string_of_int container_top ^ "px");
        main_div#_get_style#_set_marginLeft (string_of_int container_left ^ "px");
        main_div#_get_style#_set_height (string_of_int container_height ^ "px");
        main_div#_get_style#_set_width (string_of_int container_width ^ "px");
        main_div#_get_style#_set_border "solid 1px black";

        (* Create the left bar *)
        let left_bar = create_bar doc "left_bar" in

        (* Create the right bar *)
        let right_bar = create_bar doc "right_bar" in

        (* Create the ball *)
        let ball = create_ball doc "ball" in

        (* Initial status *)
        ignore (main_div#appendChild left_bar);
        move_elt left_bar left_bar_init_Y left_bar_X;
        ignore (main_div#appendChild right_bar);
        move_elt right_bar right_bar_init_Y right_bar_X;
        ignore (main_div#appendChild ball);
        move_elt ball initBallY initBallX;

        (* Launch the ball ! *)
        doc#_set_onkeypress mainDivKeyProcess;
        ignore (Dom.window#setTimeout (step (initBallX, initBallY, initBallDirection)) ballUpdateInterval);;
 

Dom.window#_set_onload onload
