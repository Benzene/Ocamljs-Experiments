(* Some config stuff *)
let bar_height = 100;;
let bar_width = 5;;
let ball_size = 5;;

let minX = 10;;
let maxX = 700;;
let minY = 10;;
let maxY = 200;;

let initBallX = 250;;
let initBallY = 50;;
let initBallDirection = "BottomRight";;
let ballMvt = 5;;

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
        rect#_get_style#_set_border "dotted 1px black";
        rect;;

let create_bar doc id =
        create_rect doc id bar_height bar_width;;

(* The type representation of the ball *)
type direction = TopLeft | TopRight | BottomLeft | BottomRight

let string_of_direction = function
        | TopLeft -> "TopLeft"
        | TopRight -> "TopRight"
        | BottomLeft -> "BottomLeft"
        | BottomRight -> "BottomRight"

let direction_of_string = function
        | "TopLeft" -> TopLeft
        | "TopRight" -> TopRight
        | "BottomLeft" -> BottomLeft
        | "BottomRight" -> BottomRight
        | _ -> BottomRight

let getBallX (ball:Dom.element) : int = 
        int_of_string ball#_get_style#_get_marginLeft

let getBallY (ball:Dom.element) : int =
        int_of_string ball#_get_style#_get_marginTop

let getBallDirection (ball:Dom.element) : direction = 
        direction_of_string ball#_get_className

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

let advance_ball (ball:Dom.element) : unit =
        let (newX, newY, newDir) = getNextBallPos (getBallX ball) (getBallY ball) (getBallDirection ball) in
        move_elt ball newY newX;
        ball#_set_className (string_of_direction newDir);; 

let step _ = 
        advance_ball (Dom.document#getElementById "ball");;
        
(* The global boot function *)                
let onload _ = 
        let doc = Dom.document in
        let main_div = doc#getElementById "main" in

        (* Create the left bar *)
        let left_bar = create_bar doc "left_bar" in

        (* Create the right bar *)
        let right_bar = create_bar doc "right_bar" in

        (* Create the ball *)
        let ball = create_ball doc "ball" in

        (* Initial status *)
        ignore (main_div#appendChild left_bar);
        move_elt left_bar 10 10;
        ignore (main_div#appendChild right_bar);
        move_elt right_bar 10 600;
        ignore (main_div#appendChild ball);
        move_elt ball initBallY initBallX;
        ball#_set_className initBallDirection;

        (* Launch the ball ! *)
        ignore (Dom.window#setInterval step 10.0);;

Dom.window#_set_onload onload
