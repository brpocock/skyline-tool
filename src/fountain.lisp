(in-package :skyline-tool)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (declaim (sb-ext:muffle-conditions sb-kernel:redefinition-warning))

  (defun all-cdrs (expr)
    "Collect all the CDRs of every CONS child of EXPR"
    (loop for x in (cdr expr)
          collecting (if (consp x)
                         (all-cdrs x)
                         x)))

  (defun variable-refs-p (expr)
    "Does EXPR contain a variable reference (other than PI or E)?"
    (some #'symbolp
          (remove 'pi (remove 'e (all-cdrs expr)))))

  (defun eval-expr (expr)
    "If EXPR is a simple arithmetic expression, reduce it to a constant"
    (assert (member (car expr) '(+ - / * expt sqrt log )))
    (apply (car expr) (mapcar (lambda (term)
                                (if (consp term)
                                    (eval-expr term)
                                    term))
                              (cdr expr))))

  (defun maybe-do-math (expr)
    "If EXPR can be reduced to a constant, do so"
    (etypecase expr
      (symbol (ecase expr
                (pi pi)
                (e (exp 1))))
      (number expr)
      (cons (if (variable-refs-p expr)
                (cons (car expr)
                      (loop for term on (cdr expr)
                            collecting (maybe-do-math term)))
                (eval-expr expr)))))

  (defgeneric cross-quarter-direction (north/south east/west)
    (:documentation "Given a symbol NORTH or SOUTH and EAST or WEST,
return the symbol for the cross-quarter direction, e.g. NORTHEAST")
    (:method ((north (eql 'north)) (east (eql 'east)))
      'northeast)
    (:method ((north (eql 'north)) (west (eql 'west)))
      'northwest)
    (:method ((south (eql 'south)) (east (eql 'east)))
      'southeast)
    (:method ((south (eql 'south)) (west (eql 'west)))
      'southwest)
    (:method ((nor/sou t) (eas/wes t))
      (error "Cross-quarter direction cannot be found from ~s ~s"
             nor/sou eas/wes)))

  (defun stage/when (_when conditional _comma clauses _stop)
    "when CONDITIONAL is true, perform CLAUSES"
    (declare (ignore _when _comma _stop))
    (list 'if conditional clauses))

  (defun stage/if-otherwise (if conditional comma clauses sem
                                otherwise otherwise-clauses stop)
    "iff CONDITIONAL is true, perform CLAUSES, otherwise, OTHERWISE-CLAUSES"
    (declare (ignore if comma sem otherwise stop))
    (list 'if conditional clauses otherwise-clauses))

  (defun stage/unless (unless conditional comma clauses stop)
    "Unless CONDITIONAL is true, perform CLAUSES."
    (declare (ignore unless comma stop))
    (list 'if conditional nil clauses))

  (defun stage/repeat (_repeat numeric _times _colon clauses)
    "Repeat CLAUSES NUMERIC times"
    (declare (ignore _repeat _times _colon))
    (list 'repeat numeric clauses))

  (defun stage/jump-to-file (_continued _in file _in_ folder)
    "script is continued in another file/asset"
    (declare (ignore _continued _in _in_))
    ;; XXX could use symbolic script IDs here for legibility of generated code
    (list 'jump (find-script-id (concatenate 'string folder "/" file))))

  (defun stage/semicolon-clauses (a sem b)
    "form a program from A followed by B"
    (declare (ignore sem))
    (list 'progn a b))

  (defun stage/do-block (do colon statements done stop)
    "treat STATEMENTS as one program"
    (declare (ignore do colon done stop))
    (list 'progn statements))

  (defun stage/statements-list (statements statement)
    "create a program from STATEMENTS followed by STATEMENT"
    (list 'progn (concatenate 'list statements statement)))

  (defun stage/truck-left/right (truck left/right)
    "Truck the camera LEFT/RIGHT by one tile"
    (declare (ignore truck))
    (list 'camera-move left/right 1))

  (defun stage/dolly-up/down (dolly up/down)
    "Dolly the camera UP/DOWN by one tile"
    (declare (ignore dolly))
    (list 'camera-move up/down 1))

  (defun stage/truck-numeric-left/right (truck numeric left/right)
    "truck the camera LEFT/RIGHT by NUMERIC tiles"
    (declare (ignore truck))
    (list 'camera-move left/right numeric))

  (defun stage/dolly-numeric-up/down (dolly numeric up/down)
    "dolly the camera UP/DOWN by NUMERIC tiles"
    (declare (ignore dolly))
    (list 'camera-move up/down numeric))

  (defun stage/camera-include (move direction to include actor/location)
    "truck/dolly the camera to include ACTOR/LOCATION"
    (declare (ignore move direction to include))
    (list 'camera-include actor/location))

  (defun stage/camera-center (move direction to center
                              on actor/location)
    "truck/dolly the camera to center on ACTOR/LOCATION"
    (declare (ignore move direction to center on))
    (list 'camera-center actor/location))

  (defun stage/camera-frame (frame actor/location and other)
    "truck/dolly the camera to frame both ACTOR/LOCATION and (when possible) also OTHER."
    (declare (ignore frame and))
    (list 'camera-frame actor/location other))

  (defun stage/camera-close (close on actor/location)
    "Center the camera on ACTOR/LOCATION"
    (declare (ignore close on))
    (list 'camera-center actor/location))

  (defun stage/one-beat (beat)
    "Wait one beat"
    (declare (ignore beat))
    (list 'wait 'beat 1))

  (defun stage/numeric-beats (numeric beat)
    "Wait NUMERIC beats"
    (declare (ignore beat))
    (list 'wait 'beat numeric))

  (defun stage/wait-secs (wait for numeric second)
    "Wait NUMERIC seconds"
    (declare (ignore wait for second))
    (list 'wait 'second numeric))

  (defun stage/wait-beats (wait for numeric beat)
    "Wait NUMERIC beats"
    (declare (ignore wait for beat))
    (list 'wait 'beat numeric))

  (defun stage/go (go quoted)
    "Go to QUOTED in the script"
    (declare (ignore go))
    (list 'go quoted))

  (defun stage/go-to (go to quoted)
    "Go to QUOTED in the script"
    (declare (ignore go to))
    (list 'go quoted))

  (defun stage/we-hear-sound (we hear sound)
    "Play SOUND"
    (declare (ignore we hear))
    (list 'hear sound))

  (defun stage/song-plays (song plays)
    "Play SONG once"
    (declare (ignore plays))
    (list 'music 'incidental song))

  (defun stage/song-starts-playing (song starts playing)
    "Begin playing SONG"
    (declare (ignore starts playing))
    (list 'music 'start song))

  (defun stage/song-stops-playing (song stops playing)
    "Stop SONG if it is playing"
    (declare (ignore stops playing))
    (list 'music 'stop song))

  (defun stage/music-stops (the music stops)
    "Stop all music"
    (declare (ignore the music stops))
    (list 'music 'stop t))

  (defun stage/set-var-to-val (set var to num)
    "assign VAR the value NUM"
    (declare (ignore set to))
    (list 'set (list 'var var) num))

  (defun stage/var←val (var from num)
    "assign VAR the value NUM"
    (declare (ignore from))
    (list 'set (list 'var var) num))

  (defun stage/var<-val (var less tack num)
    (declare (ignore less tack))
    (list 'set (list 'var var) num))

  (defun stage/set-state-to-val (set state to num)
    (declare (ignore set to))
    (list 'set state num))

  (defun stage/state←val (state from num)
    (declare (ignore from))
    (list 'set state num))

  (defun stage/state<-val (state less tack num)
    (declare (ignore less tack))
    (list 'set state num))

  (defun stage/set-flag-to-bool (set flag to bool)
    (declare (ignore set to))
    (list 'set flag bool))

  (defun stage/ship-can-go (the ship can go to place)
    (declare (ignore the ship can go to))
    (list 'ship '+ place))

  (defun stage/ship-can-not-go (the ship can not go to place)
    (declare (ignore the ship can not go to))
    (list 'ship '- place))

  (defun stage/player-gains/loses-crowns
      (the player gains/loses numeric crowns)
    (declare (ignore the player crowns))
    (list 'alter (list 'player 'crowns) gains/loses numeric))

  (defun stage/player-gains/loses-arrows
      (the player gains/loses numeric arrows)
    (declare (ignore the player arrows))
    (list 'alter (list 'player 'arrows) gains/loses numeric))

  (defun stage/player-gains/loses-karma
      (the player gains/loses numeric karma)
    (declare (ignore the player karma))
    (list 'alter (list 'player 'karma) gains/loses numeric))

  (defun stage/player-gains/loses-item (the player gains/loses the* item)
    (declare (ignore the player the*))
    (list 'inventory gains/loses item))

  (defun stage/player-gains-armor (the player gains armor numeric)
    (declare (ignore the player gains armor))
    (list 'inventory '+
          (ecase numeric
            (1 'armor1) (2 'armor2) (3 'armor3) (4 'armor4))))

  (defun stage/player-gains-ring (the player gains ring numeric)
    (declare (ignore the player gains ring))
    (list 'inventory '+
          (ecase numeric
            (1 'ring1) (2 'ring2) (3 'ring3) (4 'ring4))))

  (defun stage/player-gains/loses-quest-item
      (the player gains/loses quoted)
    (declare (ignore the player))
    (list 'inventory gains/loses quoted))

  (defun stage/gives-item (one gives the item to other)
    (declare (ignore gives the to))
    (list 'give one other item))

  (defun stage/inc/dec-state-1 (inc/dec state)
    (list inc/dec state 1 (ecase inc/dec
                            (inc #xff)
                            (dec 0))))

  (defun stage/inc/dec-state-by-n (inc/dec state by numeric)
    (declare (ignore by))
    (list inc/dec state numeric (ecase inc/dec
                                  (inc #xff)
                                  (dec 0))))

  (defun stage/inc/dec-state-by-n/limit
      (inc/dec state by numeric comma limit limiter)
    (declare (ignore by comma limit))
    (list inc/dec state numeric limiter))

  (defun stage/inc/dec-state-1/limit
      (inc/dec player-state comma limit numeric)
    (declare (ignore comma limit))
    (list inc/dec player-state 1 numeric))

  (defun stage/all-of (all of a comma and b)
    (declare (ignore all of comma and))
    (list 'every a b))

  (defun stage/any-of (any of a comma or b)
    (declare (ignore any of comma or))
    (list 'some a b))

  (defun stage/none-of (none of a comma nor b)
    (declare (ignore none of comma nor))
    (list 'not-any a b))

  (defun stage/either-or (either a comma or b)
    (declare (ignore either comma or))
    (list 'or a b))

  (defun stage/neither-nor (neither a comma not b)
    (declare (ignore neither comma not))
    (list 'not (list 'or a b)))

  (defun stage/both-and (both a comma and b)
    (declare (ignore both comma and))
    (list 'and a b))

  (defun stage/is-less-than (a is less than b)
    (declare (ignore is less than))
    (list '< a b))

  (defun stage/is-greater-than (a is gt than b)
    (declare (ignore is gt than))
    (list '> a b))

  (defun stage/is-less-than-or-equal-to (a is less than or eq to b)
    (declare (ignore is less than or eq to))
    (list '≤ a b))

  (defun stage/is-greater-than-or-equal-to (a is gt than or eq to b)
    (declare (ignore is gt than or eq to))
    (list '≥ a b))

  (defun stage/is-equal-to (a is eq to b)
    (declare (ignore is eq to))
    (list '= a b))

  (defun stage/num-is-num (a is b)
    (declare (ignore is))
    (list '= a b))

  (defun stage/is-zero (number is zero)
    (declare (ignore is zero))
    (list '= number 0))

  (defun stage/is-not-less-than (a is not less than b)
    (declare (ignore is not less than))
    (list '≥  a b))

  (defun stage/is-not-greater-than (a is not gt than b)
    (declare (ignore is not gt than))
    (list '≤ a b))

  (defun stage/is-not-equal-to (a is not eq to b)
    (declare (ignore is not eq to))
    (list '≠ a b))

  (defun stage/is-not-zero (a is not zero)
    (declare (ignore is not zero))
    (list '≠ a 0))

  (defun stage/is-pos-or-zero (a is pos or zero)
    (declare (ignore is pos or zero))
    (list '≥ a 0))

  (defun stage/is-zero-or-pos (a is zero or pos)
    (declare (ignore is zero or pos))
    (list '≥ a 0))

  (defun stage/is-pos (a is positive)
    (declare (ignore is positive))
    (list '> a 0))

  (defun stage/is-neg (a is neg)
    (declare (ignore is neg))
    (list '< a 0))

  (defun stage/< (a lt b)
    (declare (ignore lt))
    (list '< a b))

  (defun stage/> (a gt b)
    (declare (ignore gt))
    (list '> a b))

  (defun stage/<= (a lt eq b)
    (declare (ignore lt eq))
    (list '≤ a b))

  (defun stage/≤ (a lteq b)
    (declare (ignore lteq))
    (list '≤ a b))

  (defun stage/>= (a gt eq b)
    (declare (ignore gt eq))
    (list '≥ a b))

  (defun stage/≥ (a gteq b)
    (declare (ignore gteq))
    (list '≥ a b))

  (defun stage/= (a eq b)
    (declare (ignore eq))
    (list '= a b))

  (defun stage//= (a not eq b)
    (declare (ignore not eq))
    (list '≠ a b))

  (defun stage/≠ (a neq b)
    (declare (ignore neq))
    (list '≠ a b))

  (defun stage/is-between (a is between min and max)
    (declare (ignore is between and))
    (list '< min a max))

  (defun stage/is-from-to (a is from min to max)
    (declare (ignore is from to))
    (list '≤ min a max))

  (defun stage/parens (lparen expr rparen)
    (declare (ignore lparen rparen))
    expr)

  (defun stage/conditionals-list (cond comma more)
    (declare (ignore comma))
    (list 'progn cond more))

  (defun stage/exit (someone exits)
    (declare (ignore exits))
    (list 'exit someone))

  (defun stage/faces (someone faces dir)
    (declare (ignore faces))
    (list 'face someone dir))

  (defun stage/faces-to (someone faces to the dir)
    (declare (ignore faces to the))
    (list 'face someone dir))

  (defun stage/walks-relative (someone _walks relative)
    (declare (ignore _walks))
    (list 'walk someone relative))

  (defun stage/start-walk-relative (someone _starts _walking relative)
    (declare (ignore _starts _walking))
    (list 'walk someone relative :waitp nil))

  (defun stage/relative-to (to loc)
    (declare (ignore to))
    loc)

  (defun stage/relative-steps (rel comma then step)
    (declare (ignore comma then))
    (list 'progn rel step))

  (defun stage/num-to-the-dir (num to the dir)
    (declare (ignore to the))
    (list 'δ num dir))

  (defun stage/num-dir (num dir)
    (list 'δ num dir))

  (defun stage/dir-num (dir num)
    (list 'δ num dir))

  (defun stage/dir-by-num (dir by num)
    (declare (ignore by))
    (list 'δ num dir))

  (defun stage/num-ud-lr (num nor eas)
    (declare (ignore nor eas))
    (list 'δ num 'south num 'east))

  (defun stage/ud-lr-by-num (nor eas by num)
    (declare (ignore nor eas by))
    (list 'δ num 'south num 'east))

  (defun stage/relative-ud-lr (n1 up/down and n2 left/right)
    (declare (ignore and))
    (list 'δ n1 up/down n2 left/right))

  (defun stage/relative-lr-ud (n1 left/right and n2 up/down)
    (declare (ignore and left/right up/down))
    (list 'δ n1 'east n2 'south))

  (defun stage/speech-params
      (actor speaks with pitch p comma speed s)
    (declare (ignore speaks with pitch comma speed))
    (list 'voice actor p s))

  (defun stage/npc-desc-2 (d1 and d2)
    (declare (ignore and))
    (list d1 d2))

  (defun stage/npc-desc-3 (d1 comma1 d2 comma2 and d3)
    (declare (ignore comma1 comma2 and))
    (list d1 d2 d3))

  (defun stage/npc-desc-4 (d1 c1 d2 c2 d3 c3 and d4)
    (declare (ignore c1 c2 c3 and))
    (list d1 d2 d3 d4))

  (defun stage/color-hair (color hair)
    (declare (ignore hair))
    (list 'hair color))

  (defun stage/color-skin (color skin)
    (declare (ignore skin))
    (list 'skin color))

  (defun stage/color-tunic (a color tunic)
    (declare (ignore a tunic))
    (list 'tunic color))

  (defun stage/color-robe (a color robe)
    (declare (ignore a robe))
    (list 'robe color))

  (defun stage/head-num (head number)
    (declare (ignore head))
    (list 'head number))

  (defun stage/num-dir-of-place (num dir of quoted)
    (declare (ignore of))
    (list 'place quoted num dir))

  (defun stage/num-lr-up-of-place (num lr and num2 ud of quoted)
    (declare (ignore and of))
    (list 'place quoted num lr num2 ud))

  (defun stage/num-up-lr-of-place (num ud and num2 lr of quoted)
    (declare (ignore and of))
    (list 'place quoted num2 lr num ud))

  (defun stage/raw-coords (lparen x comma y rparen)
    (declare (ignore lparen comma rparen))
    (list 'place nil x 'east y 'south))

  (defun stage/complex (lparen real plus imag i rparen)
    (declare (ignore lparen plus i rparen))
    (complex real imag))

  (defun stage/sum-of-n+n (the sum of n1 plus n2)
    (declare (ignore the sum of plus))
    (list '+ n1 n2))

  (defun stage/diff-of-n---n (the diff of n1 minus n2)
    (declare (ignore the diff of minus))
    (list '- n1 n2))

  (defun stage/quot-of-n÷n (the quot of n1 div n2)
    (declare (ignore the quot of div))
    (list '/ n1 n2))

  (defun stage/prod-of-n×n (the prod of n1 times n2)
    (declare (ignore the prod of times))
    (list '* n1 n2))

  (defun stage/num-ash-num (the res of n1 shif by n2)
    (declare (ignore the res of shif by))
    (list 'ash n1 n2))

  (defun stage/num-expt-num (the res of n1 raised to the2 n2 pow)
    (declare (ignore the res of raised to the2 pow))
    (list 'expt n1 n2))

  (defun stage/log-base-n-of-n (the log base n2 of n1)
    (declare (ignore the log base of))
    (list 'log n1 n2))

  (defun stage/base-n-log-of-n (the base n2 log of n1)
    (declare (ignore the base log of))
    (list 'log n1 n2))

  (defun stage/nat-log-of-n (the nat log of n)
    (declare (ignore the nat log of))
    (list 'log n))

  (defun stage/sqrt-of-n (the sq rt of num)
    (declare (ignore the sq rt of))
    (list 'sqrt num))

  (defun stage/ceiling-n (the ceil val of num)
    (declare (ignore the ceil val of))
    (list 'ceiling num))

  (defun stage/floor-n (the floor val of num)
    (declare (ignore the floor val of))
    (list 'floor num))

  (defun stage/round-n (the round val of num)
    (declare (ignore the round val of))
    (list 'round num))

  (defun stage/realpart-n (the real part of num)
    (declare (ignore the real part of))
    (list 'realpart num))

  (defun stage/imagpart-n (the imag part of num)
    (declare (ignore the imag part of))
    (list 'imagpart num))

  (defun stage/abs-n (the abs val of num)
    (declare (ignore the abs val of))
    (list 'abs num))

  (defun stage/bool-and (bool n1 and n2)
    (declare (ignore bool and))
    (list 'logand n1 n2))

  (defun stage/bool-or (bool n1 or n2)
    (declare (ignore bool or))
    (list 'logior n1 n2))

  (defun stage/bool-xor (bool ex n1 or n2)
    (declare (ignore bool ex or))
    (list 'logxor n1 n2))

  (defun stage/logior (bool incl n1 or n2)
    (declare (ignore bool incl or))
    (list 'logior n1 n2))

  (defun stage/call-with-regs (call subroutine with registers)
    (declare (ignore call with))
    (list 'progn registers (list 'jsr subroutine)))

  (defun stage/call (call subroutine)
    (declare (ignore call))
    (list 'jsr subroutine))

  (defun stage/regs-list (regs comma reg)
    (declare (ignore comma))
    (list 'progn regs reg))

  (defun stage/move-into-reg (reg eq num)
    (declare (ignore eq))
    (list 'mv reg num))

  (defun stage/enter (enter someone at place)
    (declare (ignore enter at))
    (list 'enter someone place))

  (defun stage/empty-boat (the ship-name appears in _the east/west headed for actor/location)
    (declare (ignore the appears in _the headed for))
    (list 'boat ship-name east/west actor/location nil))

  (defun stage/full-boat (the ship-name appears in _the east/west with actors aboard
                          headed to/for actor/location)
    (declare (ignore the appears in _the with aboard headed to/for))
    (list 'boat ship-name east/west actor/location actors))

  (defun stage/sail-away (the ship-name gets under weigh to _the east/west)
    (declare (ignore the gets under weigh to _the))
    (list 'sail-away ship-name east/west))

  (defun stage/embarks (actor embarks/boards the ship-name)
    (declare (ignore embarks/boards the))
    (list 'embark actor ship-name))

  (defun stage/disembarks (actor disembarks* the ship-name)
    (declare (ignore disembarks* the))
    (list 'disembark actor ship-name)))

(define-constant +stage-direction-words+
    (mapcar (lambda (x) (intern (symbol-name x) #.*package*))
            '(|(| |)| + |,| - |.| /  × ÷ skyline-tool::|:| |…|
              a an aboard above absolute alarmed all amulet and appears arrow arrows arms armor at awakens
              base beat beats becomes below black boards boolean boots
              both bow bright brightly buckler by
              can catamaran ceiling chalice clear close confused continued crown crowns cut cyan cyan-lit
              dances dancing dark difference dim disembarks divided do dolly done down durbat
              e east either embarks enter enters equal equips exclusive exit exits
              faces fade find floor for frame from
              flies flying
              gains gets glass go goes grand grappling-hook greater
              gestures gesturing
              hair hammer has head headed hear her here his hp hurt
              if imaginary in include inclusive is it
              knife
              large launch left less like lit logand logarithm logical
              logior logxor looks loses
              magic mask minus moves moving
              natural negative next night none nor normal-lit normally north not nothing
              of on open or
              panics panicking
              part pi picks pirate pitch player-armor-color
              player-hair-color player-skin-color playing plays plus
              potion positive power product purple
              quickly quotient
              raining raise raised ready real red red-lit repeat right ring robe rope root round rowboat
              second seconds see set shadow shield shift ship sleeps sleep
              skin sloop slowly small staff south
              square starts stops suddenly sum surprised sweating sword
              than the their then times to torch truck tunic
              under unless up upon
              value
              wait walking walks wand we weigh west when white with wrench wakes
              waves waving
              yellow
              zero))
  :test 'equalp
  :documentation "Words recognized specially by the stage directions parser")

(setf *stage-direction-parser* nil)

(eval
 `(yacc:define-parser *stage-direction-parser*
    (:start-symbol directions)
    (:terminals #.(concatenate 'list +stage-direction-words+
                               '(number quoted actor variable)
                               *common-palette*))
    (:precedence ((:right -) (:left + -) (:left * /)
                  (:left directions)
                  (:left statement)
                  (:left preparation-paragraph)))
    (directions (statement #'identity)
                (directions statement #'list))
    (someone actor
             (the actor #1=(lambda (_the actor) (declare (ignore _the)) actor))
             (a actor #1#)
             (an actor #1#))
    (statement call-expr
               (when conditional |,| clauses |.|
                     #'stage/when)
               (if conditional |,| clauses |.|
                   #'stage/when)
               (if conditional |,| clauses skyline-tool::|;| otherwise |,| clauses |.|
                   #'stage/if-otherwise)
               (unless conditional |,| clauses |.|
                       #'stage/unless)
               (clauses |.| (lambda (clauses _stop)
                              (declare (ignore _stop))
                              clauses))
               (repeat numeric times skyline-tool::|:| clauses
                       #'stage/repeat)
               preparation-paragraph)
    (preparation-paragraph (preparation-introduction ellipsis directions preparation-closing ellipsis
                          	                       (lambda (_intro _ellipsis directions _closing _ellipsout)
                                                       (declare (ignore _intro _ellipsis _closing _ellipsout))
                                                       (list 'prepare directions))))
    (preparation-introduction (we open on) (open on) (we find) (we see))
    (preparation-closing then suddenly next)
    (ellipsis (|.| |.| |.|) |…| skyline-tool::|:|)
    (clauses clause
             sem-clauses
             (clause |,| and then clause (lambda (clause1 _comma _and _then clause2)
                                           (declare (ignore _comma _and _then))
                                           (list 'progn clause1 clause2)))
             (clause |,| and clause (lambda (clause1 _comma _and clause2)
                                      (declare (ignore _comma _and))
                                      (list 'progn clause1 clause2)))
             do/done-block)
    (sem-clauses (clause skyline-tool::|;| clause
                         #'stage/semicolon-clauses)
                 (sem-clauses skyline-tool::|;| clause
                              #'stage/semicolon-clauses))
    (clause beat-clause
            fade-clause
            wake/sleep-clause
            dance-clause
            enter-clause
            equip-clause
            pick-up-clause
            actor-is-clause
            exit-clause
            walk-clause
            facing-clause
            audio-clause
            assignment-clause
            go-to-clause
            truck/dolly
            ship-clause
            weather-clause
            lighting-clause
            (cut to include actor/location)
            (cut to center on actor/location)
            (at numeric / second |,| truck/dolly)
            jump-to-other-file-clause)

    (actor-is-clause (someone is actor-coda
                              (lambda (someone _is coda)
                                (declare (ignore _is))
                                (cons (car coda)
                                      (cons someone (rest coda)))))
                     (someone looks actor-condition
                              (lambda (someone _is coda)
                                (declare (ignore _is))
                                (cons (car coda)
                                      (cons someone (rest coda)))))
                     (someone is actor-condition
                              (lambda (someone _is coda)
                                (declare (ignore _is))
                                (cons (car coda) (cons someone (rest coda))))))
    (actor-coda
     (at location (lambda (_at location)
                    (declare (ignore _at))
                    (list 'enter location)))
     (hurt (lambda (_hurt)
             (declare (ignore _hurt))
             (list 'hurt 1)))
     (hurt for number hp (lambda (_hurt _for number _hp)
                           (declare (ignore _hurt _for _hp))
                           (list 'hurt number)))
     (sweating (lambda (_sweating)
                 (declare (ignore _sweating))
                 (list 'emote '?)))
     (confused (lambda (_confused)
                 (declare (ignore _confused))
                 (list 'emote '?)))
     (surprised (lambda (_surprised)
                  (declare (ignore _surprised))
                  (list 'emote '!)))
     (puzzled (lambda (_surprised)
                (declare (ignore _surprised))
                (list 'emote '?)))
     (gesturing (lambda (_gesturing)
                  (declare (ignore _gesturing))
                  (list 'gesture)))
     (dancing (lambda (_dancing)
                (declare (ignore _dancing))
                (list 'dance)))
     (flying (lambda (_flying)
               (declare (ignore _flying))
               (list 'fly)))
     (panicking (lambda (_panicking)
                  (declare (ignore _panicking))
                  (list 'panic)))
     (waving (lambda (_waving)
               (declare (ignore _waving))
               (list 'wave-arms))))

    (actor-condition
     (sweating (lambda (_sweating)
                 (declare (ignore _sweating))
                 (list 'emote 'sweat)))
     (confused (lambda (_confused)
                 (declare (ignore _confused))
                 (list 'emote '?)))
     (surprised (lambda (_surprised)
                  (declare (ignore _surprised))
                  (list 'emote '!)))
     (alarmed (lambda (_surprised)
                (declare (ignore _surprised))
                (list 'emote '!)))
     (puzzled (lambda (_surprised)
                (declare (ignore _surprised))
                (list 'emote '?))))

    (pick-up-clause (actor picks up article quoted
                           (lambda (actor _picks _up _an item)
                             (declare (ignore _picks _up _an))
                             (list 'pick-up actor item)))
                    (actor picks up quoted
                           (lambda (actor _picks _up item)
                             (declare (ignore _picks _up))
                             (list 'pick-up actor item))))

    (equip-clause (actor equips item-name
                         (lambda (actor _equips item)
                           (declare (ignore _equips))
                           (list 'equip actor item)))
                  (actor equips article item-name
                         (lambda (actor _equips _article item)
                           (declare (ignore _equips _article))
                           (list 'equip actor item))))

    (item-name nothing knife shield (small shield)
               hammer potion sword (large shield) (no shield)
               bow torch chalice staff wand rope glass wrench) ; TODO all items

    (article a an the)

    (around-here here
                 out
                 (around here))

    (weather-condition raining)

    (weather-clause (it is clear (lambda (&rest _)
                                   (declare (ignore _))
                                   (list 'weather nil)))
                    (it is weather-condition (lambda (_it _is weather)
                                               (declare (ignore _it _is))
                                               (list 'weather weather)))
                    (it is clear around-here (lambda (&rest _)
                                               (declare (ignore _))
                                               (list 'weather nil)))
                    (it is weather-condition around-here
                        (lambda (_it _is weather _here)
                          (declare (ignore _it _is _here))
                          (list 'weather weather))))
    (lighting-word (dim (constantly 'dark))
                   (night (constantly 'dark))
                   dark
                   bright
                   red-lit
                   cyan-lit
                   normal-lit
                   (normally lit (constantly 'normal-lit))
                   (brightly lit (constantly 'bright))
                   (dimly lit (constantly 'dark)))
    (speed-adverb slowly quickly)
    (lighting-clause (it is lighting-word
                         (lambda (_it _is lighting)
                           (declare (ignore _it _is))
                           (list 'lighting lighting)))
                     (it is lighting-word around-here
                         (lambda (_it _is lighting &rest _)
                           (declare (ignore _it _is _))
                           (list 'lighting lighting)))
                     (it becomes lighting-word
                         (lambda (_it _becomes lighting &rest _)
                           (declare (ignore _it _becomes _))
                           (list 'lighting-change lighting 'normal)))
                     (it becomes lighting-word around-here
                         (lambda (_it _becomes lighting &rest _)
                           (declare (ignore _it _becomes _))
                           (list 'lighting-change lighting 'normal)))
                     (it speed-adverb becomes lighting-word
                         (lambda (_it speed _becomes lighting &rest _)
                           (declare (ignore _it _becomes _))
                           (list 'lighting-change lighting speed)))
                     (it speed-adverb becomes lighting-word around-here
                         (lambda (_it speed _becomes lighting &rest _)
                           (declare (ignore _it _becomes _))
                           (list 'lighting-change lighting speed))))

    (wake/sleep-clause (someone sleeps (lambda (someone &rest _)
                                         (declare (ignore _))
                                         (list 'sleep someone)))
                       (someone wakes up (lambda (someone &rest _)
                                           (declare (ignore _))
                                           (list 'wake someone)))
                       (someone awakens (lambda (someone &rest _)
                                          (declare (ignore _))
                                          (list 'wake someone))))

    (fade-color black white red cyan)
    (fade-clause (fade from fade-color (lambda (_fade _from color)
                                         (declare (ignore _fade _from))
                                         (list 'fade-in color)))
                 (fade to fade-color (lambda (_fade _to color)
                                       (declare (ignore _fade _to))
                                       (list 'fade-out color))))

    (to/for to for)

    (ship-name quoted)

    (east/west (east (constantly 'east)) (west (constantly 'west)))

    (someone-list (someone |,| someone)
                  (someone-list |,| someone))

    (someones someone
              (someone and someone)
              (someone |,| and someone)
              (someone-list |,| and someone))

    (call-expr (call quoted
                     #'stage/call)
               (call quoted with registers
                     #'stage/call-with-regs))
    (registers register
               (registers |,| register
                          #'stage/regs-list))
    (register (reg-name = numeric
                        #'stage/move-into-reg))
    (reg-name a x y)

    (do/done-block (do |:| statements done |.|
                     #'stage/do-block))
    (statements statement
                (statements statement
                            #'stage/statements-list))

    (truck/dolly (truck left/right
                        #'stage/truck-left/right)
                 (dolly up/down
                        #'stage/dolly-up/down)
                 (truck numeric left/right
                        #'stage/truck-numeric-left/right)
                 (dolly numeric up/down
                        #'stage/dolly-numeric-up/down)
                 (dolly/truck direction to include actor/location
                              #'stage/camera-include)
                 (dolly/truck direction to center on actor/location
                              #'stage/camera-center)
                 (frame actor/location and actor/location
                        #'stage/camera-frame)
                 (close on actor/location
                        #'stage/camera-close))
    (actor/location someone location)
    (beat-clause (beat #'stage/one-beat)
                 (numeric beat
                          #'stage/numeric-beats)
                 (numeric beats
                          #'stage/numeric-beats)
                 (wait for actor (lambda (_wait _for actor)
                                   (declare (ignore _wait _for))
                                   (list 'wait-for actor)))
                 (wait for the actor (lambda (_wait _for _the actor)
                                       (declare (ignore _wait _for _the))
                                       (list 'wait-for actor)))
                 (wait for numeric second
                       #'stage/wait-secs)
                 (wait for numeric seconds
                       #'stage/wait-secs)
                 (wait for numeric beat
                       #'stage/wait-beats)
                 (wait for numeric beats
                       #'stage/wait-beats))
    (go-to-clause (go quoted
                      #'stage/go)
                  (go to quoted
                      #'stage/go-to))
    (audio-clause (we hear quoted
                      #'stage/we-hear-sound)
                  (quoted starts playing
                          #'stage/song-starts-playing)
                  (quoted plays
                          #'stage/song-plays)
                  (the music stops
                       #'stage/music-stops)
                  (silence
                   #'first))
    (assignment-clause (set variable to numeric
                            #'stage/set-var-to-val)
                       (variable ← numeric
                                 #'stage/var←val)
                       (variable < - numeric
                                 #'stage/var<-val)
                       (set someone-state to numeric
                            #'stage/set-state-to-val)
                       (someone-state ← numeric
                                      #'stage/state←val)
                       (someone-state < - numeric
                                      #'stage/state<-val)
                       (set player-state to numeric
                            #'stage/set-state-to-val)
                       (player-state ← numeric
                                     #'stage/state←val)
                       (player-state < - numeric
                                     #'stage/state<-val)
                       (set quoted to true/false
                            #'stage/set-flag-to-bool)
                       (the ship can go to quoted
                            #'stage/ship-can-go)
                       (the ship can not go to quoted
                            #'stage/ship-can-not-go)
                       inc/dec-expr)
    (true/false (true (constantly t))
                (yes (constantly t))
                (on (constantly t))
                (false (constantly nil))
                (no (constantly nil))
                (off (constantly nil)))
    (inc/dec-expr (the player gains/loses numeric crowns
                       #'stage/player-gains/loses-crowns)
                  (the player gains/loses numeric arrows
                       #'stage/player-gains/loses-arrows)
                  (the player gains/loses numeric karma
                       #'stage/player-gains/loses-karma)
                  (the player gains/loses the item
                       #'stage/player-gains/loses-item)
                  (the player gains armor numeric
                       #'stage/player-gains-armor)
                  (the palyer gains ring numeric
                       #'stage/player-gains-ring)
                  (the player gains/loses quoted
                       #'stage/player-gains/loses-quest-item)
                  (someone gives the item to someone
                           #'stage/gives-item)
                  (inc/dec player-state
                           #'stage/inc/dec-state-1)
                  (inc/dec player-state by numeric
                           #'stage/inc/dec-state-by-n)
                  (inc/dec player-state by numeric |,| limit numeric
                           #'stage/inc/dec-state-by-n/limit)
                  (inc/dec player-state |,| limit numeric
                           #'stage/inc/dec-state-1/limit))
    (item knife buckler hammer amulet potion sword shield bow
          torch chalice staff wand grappling-hook glass wrench boots
          mask glove)
    (gains/loses (gains (constantly '+))
                 (loses (constantly '-)))
    (inc/dec (increment (constantly 'inc))
             (increase (constantly 'inc))
             (decrement (constantly 'dec))
             (decrease (constantly 'dec)))
    (conditional simple-conditional
                 compound-conditional)
    (compound-conditional (all of conditionals |,| and simple-conditional
                               #'stage/all-of)
                          (any of conditionals |,| or simple-conditional
                               #'stage/any-of)
                          (none of conditionals |,| nor simple-conditional
                                #'stage/none-of)
                          (either simple-conditional |,| or simple-conditional
                                  #'stage/either-or)
                          (neither simple-conditional |,| nor simple-conditional
                                   #'stage/neither-nor)
                          (both simple-conditional |,| and simple-conditional
                                #'stage/both-and))
    (simple-conditional (numeric is less than numeric
                                 #'stage/is-less-than)
                        (numeric is greater than numeric
                                 #'stage/is-greater-than)
                        (numeric is less than or equal to numeric
                                 #'stage/is-less-than-or-equal-to)
                        (numeric is greater than or equal to numeric
                                 #'stage/is-greater-than-or-equal-to)
                        (numeric is equal to numeric
                                 #'stage/is-equal-to)
                        (numeric is number
                                 #'stage/num-is-num)
                        (numeric is zero
                                 #'stage/is-zero)
                        (numeric is not less than numeric
                                 #'stage/is-not-less-than)
                        (numeric is not greater than numeric
                                 #'stage/is-not-greater-than)
                        (numeric is not equal to numeric
                                 #'stage/is-not-equal-to)
                        (numeric is not zero
                                 #'stage/is-not-zero)
                        (numeric is positive or zero
                                 #'stage/is-pos-or-zero)
                        (numeric is zero or positive
                                 #'stage/is-zero-or-pos)
                        (numeric is positive
                                 #'stage/is-pos)
                        (numeric is negative
                                 #'stage/is-neg)
                        (numeric < numeric
                                 #'stage/<)
                        (numeric > numeric
                                 #'stage/>)
                        (numeric < = numeric
                                 #'stage/<=)
                        (numeric ≤ numeric
                                 #'stage/≤)
                        (numeric > = numeric
                                 #'stage/>=)
                        (numeric ≥ numeric
                                 #'stage/≥)
                        (numeric = numeric
                                 #'stage/=)
                        (numeric / = numeric
                                 #'stage//=)
                        (numeric ≠ numeric
                                 #'stage/≠)
                        (numeric is between numeric and numeric
                                 #'stage/is-between)
                        (numeric is from numeric to numeric
                                 #'stage/is-from-to)
                        (|(| compound-conditional |)|
                             #'stage/parens))
    (conditionals (simple-conditional |,| conditionals
                                      #'stage/conditionals-list))
    (enter-clause (skyline-tool::enter someone at location
                                       #'stage/enter))
    (ship-clause
     (the ship-name is at location
          (lambda (_the ship-name _is _at location)
            (list 'boat ship-name 'at location nil)))
     (the ship-name appears in the east/west headed for actor/location
          #'stage/empty-boat)
     (the ship-name appears in the east/west with someones aboard headed to/for actor/location
          #'stage/full-boat)
     (the ship-name gets under weigh to the east/west
          #'stage/sail-away)
     (someone embarks/boards the ship-name #'stage/embarks)
     (someone disembarks from the ship-name #'stage/disembarks))

    (embarks/boards boards (embarks upon) (embarks on))

    (exit-clause (someone exits #'stage/exit)
                 (exit someone (lambda (e s) (stage/exit s e))))
    (jump-to-other-file-clause (continued in quoted in quoted #'stage/jump-to-file))
    (facing-clause (someone faces direction
                            #'stage/faces)
                   (someone faces to the direction
                            #'stage/faces-to))
    (walk-clause (someone walks relative-position
                          #'stage/walks-relative)
                 (someone moves relative-position
                          #'stage/walks-relative)
                 ;; Start patterns
                 (someone starts walking relative-position
                          #'stage/start-walk-relative)
                 (someone starts moving relative-position
                          #'stage/start-walk-relative)
                 (someone starts dancing (lambda (someone &rest _)
                                           (declare (ignore _))
                                           (list 'dance someone)))
                 (someone starts flying (lambda (someone &rest _)
                                          (declare (ignore _))
                                          (list 'fly someone)))
                 (someone starts waving (lambda (someone &rest _)
                                          (declare (ignore _))
                                          (list 'wave-arms someone)))
                 (someone starts waving arms (lambda (someone &rest _)
                                               (declare (ignore _))
                                               (list 'wave-arms someone)))
                 (someone starts waving his arms (lambda (someone &rest _)
                                                   (declare (ignore _))
                                                   (list 'wave-arms someone)))
                 (someone starts waving her arms (lambda (someone &rest _)
                                                   (declare (ignore _))
                                                   (list 'wave-arms someone)))
                 (someone starts waving their arms (lambda (someone &rest _)
                                                     (declare (ignore _))
                                                     (list 'wave-arms someone)))
                 (someone starts gesturing (lambda (someone &rest _)
                                             (declare (ignore _))
                                             (list 'gesture someone)))
                 (someone starts panicking (lambda (someone &rest _)
                                             (declare (ignore _))
                                             (list 'panic someone)))
                 ;; Stop patterns
                 (someone stops dancing (lambda (someone &rest _)
                                          (declare (ignore _))
                                          (list 'wake someone)))
                 (someone stops flying (lambda (someone &rest _)
                                         (declare (ignore _))
                                         (list 'wake someone)))
                 (someone stops waving (lambda (someone &rest _)
                                         (declare (ignore _))
                                         (list 'wake someone)))
                 (someone stops waving arms (lambda (someone &rest _)
                                              (declare (ignore _))
                                              (list 'wake someone)))
                 (someone stops gesturing (lambda (someone &rest _)
                                            (declare (ignore _))
                                            (list 'wake someone)))
                 (someone stops panicking (lambda (someone &rest _)
                                            (declare (ignore _))
                                            (list 'wake someone)))
                 ;; Action patterns
                 (someone dances (lambda (someone &rest _)
                                   (declare (ignore _))
                                   (list 'dance someone)))
                 (someone flies (lambda (someone &rest _)
                                  (declare (ignore _))
                                  (list 'fly someone)))
                 (someone waves (lambda (someone &rest _)
                                  (declare (ignore _))
                                  (list 'wave-arms someone)))
                 (someone waves arms (lambda (someone &rest _)
                                       (declare (ignore _))
                                       (list 'wave-arms someone)))
                 (someone waves his arms (lambda (someone &rest _)
                                           (declare (ignore _))
                                           (list 'wave-arms someone)))
                 (someone waves her arms (lambda (someone &rest _)
                                           (declare (ignore _))
                                           (list 'wave-arms someone)))
                 (someone waves their arms (lambda (someone &rest _)
                                             (declare (ignore _))
                                             (list 'wave-arms someone)))
                 (someone gestures (lambda (someone &rest _)
                                     (declare (ignore _))
                                     (list 'gesture someone)))
                 (someone panics (lambda (someone &rest _)
                                   (declare (ignore _))
                                   (list 'panic someone))))
    (relative-position step-distance
                       (to location
                           #'stage/relative-to)
                       (relative-position |,| then step-distance
                                          #'stage/relative-steps))
    (step-distance (numeric to the direction
                            #'stage/num-to-the-dir)
                   (numeric direction
                            #'stage/num-dir)
                   (direction numeric
                              #'stage/dir-num)
                   (direction by numeric
                              #'stage/dir-by-num)
                   (numeric up/down left/right
                            #'stage/num-ud-lr)
                   (up/down left/right by numeric
                            #'stage/ud-lr-by-num)
                   (numeric up/down and numeric left/right
                            #'stage/relative-ud-lr)
                   (numeric left/right and numeric up/down
                            #'stage/relative-lr-ud))

    (color clear ,@*common-palette*)
    (location (quoted (lambda (place)
                        (list 'place place 0 'north 0 'east)))
              (numeric direction of/from quoted
                       #'stage/num-dir-of-place)
              (numeric left/right and numeric up/down of/from quoted
                       #'stage/num-lr-up-of-place)
              (numeric up/down and numeric left/right of/from quoted
                       #'stage/num-up-lr-of-place)
              (|(| numeric |,| numeric |)|
                   #'stage/raw-coords))
    (of/from of from)
    (direction left/right up/down)
    (left/right (left (constantly 'west))
                (right (constantly 'east))
                (east (constantly 'east))
                (west (constantly 'west)))
    (up/down (up (constantly 'north))
             (down (constantly 'south))
             (north (constantly 'north))
             (south (constantly 'south)))
    (numeric (variable (lambda (var)
                         (list 'var (subseq var 1))))
             number
             pi
             (e (constantly (exp 1)))
             someone-state
             player-state
             (- number)
             (|(| number + number i |)|
                  #'stage/complex)
             (|(| numeric |)|
                  #'stage/parens)
             (the sum of numeric plus* numeric
                  #'stage/sum-of-n+n)
             (the difference of numeric minus* numeric
                  #'stage/diff-of-n---n)
             (the quotient of numeric division* numeric
                  #'stage/quot-of-n÷n)
             (the product of numeric times* numeric
                  #'stage/prod-of-n×n)
             (the result of numeric shifted by numeric
                  #'stage/num-ash-num)
             (the result of numeric raised to the numeric power
                  #'stage/num-expt-num)
             (the logarithm base numeric of numeric
                  #'stage/log-base-n-of-n)
             (the base numeric logarithm of numeric
                  #'stage/base-n-log-of-n)
             (the natural logarithm of n
                  #'stage/nat-log-of-n)
             (the square root of numeric
                  #'stage/sqrt-of-n)
             (the ceiling value of numeric
                  #'stage/ceiling-n)
             (the floor value of numeric
                  #'stage/floor-n)
             (the round value of numeric
                  #'stage/round-n)
             (the real part of numeric
                  #'stage/realpart-n)
             (the imaginary part of numeric
                  #'stage/imagpart-n)
             (the absolute value of numeric
                  #'stage/abs-n)
             (boolean numeric and numeric
                      #'stage/bool-and)
             (boolean numeric or numeric
                      #'stage/bool-or)
             (boolean exclusive numeric or numeric
                      #'stage/bool-xor)
             (boolean inclusive numeric or numeric
                      #'stage/logior))
    (someone-state
     (the x position of someone)
     (the y position of someone)
     (the hit points of someone)
     (the max hit points of someone))
    (player-state
     (the x position of the player)
     (the y position of the player)
     (the hit points of the player)
     (the max hit points of the player)
     (the karma of the player)
     (the magic points of the player)
     (the max magic points of te player)
     (the arrows of the player)
     (the crowns of the player))
    (plus* plus +)
    (minus* minus - less)
    (division* (divided by) / ÷)
    (times* times ✕ ×)))

(defvar *fountain-state* nil)

(defun regex-match (regex string)
  "Returns a matching string to REGEX from STRING"
  (cl-ppcre:do-matches-as-strings (x regex string)
    (when x (return-from regex-match x)))
  nil)

(defun parse-stage-directions (string)
  "Parse stage directions in STRING into assembly code"
  (with-input-from-string (stream string)
    (yacc:parse-with-lexer
     (lambda () (stage-direction-lexer stream))
     *stage-direction-parser*)))

(defun fountain-lexer/parse-line (line)
  "Parse a LINE from a Fountain script"
  #+() (format *trace-output* "~%~5tPARSE: (~s) ~s" (car *fountain-state*) line)
  (block nil
    (when (null line)
      (return (list 'end nil)))
    (ecase (car *fountain-state*)
      (:continued-note
       (labels ((append-line ()
                  (concatenate 'string
                               (cdr *fountain-state*)
                               #.(format nil "~%~10t;; ")
                               line)))
         (cond
           ((search "]]" line)
            (return (prog1
                        (list 'comment (append-line))
                      (setf *fountain-state* nil))))
           (t (setf (cdr *fountain-state*) (append-line))
              (return nil)))))
      (:continued-metadata
       (cond
         ((emptyp (string-trim #(#\Space #\Tab) line))
          (return (list 'metadata
                        (prog1 (cdr *fountain-state*)
                          (setf *fountain-state* nil)))))
         ((char= #\Space (char line 0))
          (setf (cdr *fountain-state*)
                (concatenate 'string (cdr *fountain-state*)
                             (if (or (char= #\Space (last-elt (cdr *fountain-state*)))
                                     (char= #\: (last-elt (cdr *fountain-state*))))
                                 " "
                                 "; ")
                             (string-trim #(#\Space #\Tab) line)))
          (return nil))
         (t (setf *fountain-state* nil)
            (fountain-lexer/parse-line line))))
      (:stage
       (cond
         ((emptyp (string-trim #(#\Space #\Tab) line))
          (return (prog1
                      (let ((*last-values* nil))
                        (list 'stage
                              (parse-stage-directions (cdr *fountain-state*))))
                    (setf *fountain-state* nil))))
         (t (setf (cdr *fountain-state*)
                  (concatenate 'string (cdr *fountain-state*) " " line))
            (return nil))))
      (:branch
       (cond
         ((or (emptyp (string-trim #(#\Space #\Tab) line))
              (and (char= #\( (char line 0))
                   (char= #\) (last-elt line))))
          (error "Branch to ~s missing statement" (cdr *fountain-state*)))
         (t (return (prog1
                        (list 'branch (cons (cdr *fountain-state*) line))
                      (setf *fountain-state* (cons :speech "")))))))
      (:speech
       (cond
         ((or (null line) (emptyp (string-trim #(#\Space #\Tab) line)))
          (return (list 'speech (prog1 (cdr *fountain-state*)
                                  (setf *fountain-state* nil)))))
         ((and (char= #\( (char line 0))
               (char= #\) (last-elt line)))
          (return (prog1
                      (if (starts-with-subseq "(to " line)
                          (let ((label (subseq line 4 (1- (length line)))))
                            (setf *fountain-state* (cons :branch label))
                            (return nil))
                          (error "Emotes are not supported, saw ~s" line)))))
         (t (setf (cdr *fountain-state*)
                  (if (emptyp (cdr *fountain-state*))
                      line
                      (concatenate 'string (cdr *fountain-state*)
                                   (string #\Space) line)))
            (return nil))))
      ((nil)
       (cond
         ((null line)
          #| Compiler says this line is unreachable, but I'm being silly and leaving it in for now. |#
          (return (list 'end nil)))
         ((emptyp (string-trim #(#\Space #\Tab) line))
          nil)
         ((char= #\# (char line 0))
          (return (list 'label (string-trim #(#\Space #\Tab)
                                            (string-upcase (subseq line 1))))))
         ((char= #\= (char line 0))
          (return (list 'comment (concatenate 'string "Synopsis: "
                                              (subseq line 1)))))
         ((and (position #\: line)
               (destructuring-bind (key value) (split-sequence #\: line :count 2)
                 (declare (ignore value))
                 (member key (list "title" "credit" "author" "format"
                                   "source" "date" "contact")
                         :test #'string-equal)))
          (destructuring-bind (key value) (split-sequence #\: line :count 2)
            (declare (ignore key))
            (if (emptyp value)
                (return (prog1
                            nil
                          (setf *fountain-state* (cons :continued-metadata line))))
                (return (list 'metadata line)))))
         ((char= #\> (char line 0))
          (let ((out (string-upcase (string-trim #(#\Space #\Tab #\.) (subseq line 1)))))
            (loop for (start finish)
                    on (list "TO " (lambda (rest) (list 'go rest))
                             "FADE OUT" (lambda (rest)
                                          (declare (ignore rest))
                                          (list 'fade-to 'black))
                             "FADE TO" (lambda (rest)
                                         (list 'fade-to rest))
                             "FINIS" #1=(lambda (rest)
                                          (list 'end (remove-if-not #'alpha-char-p rest)))
                             "THE END" #1#
                             "END" #1#
                             "END OF LINE" (lambda (rest)
                                             (unless (emptyp rest)
                                               (cerror "Continue, ignoring extra"
                                                       "END OF LINE does not expect additional ~s" rest))
                                             '(reboot))
                             "RETURN TO TITLE " (lambda (rest)
                                                  (declare (ignore rest))
                                                  (list 'game-over "TITLE"))
                             "GAME OVER -" (lambda (rest)
                                             (list 'game-over rest)))
                  by #'cddr
                  do (multiple-value-bind (truth rest) (starts-with-subseq start out :return-suffix t)
                       (when truth
                         (return-from fountain-lexer/parse-line
                           (funcall finish (string-trim #(#\Space #\Tab) rest))))))
            (cerror "Continue, replacing with a >FINIS."
                    "Unknown transition out: ~s" line)
            (return (list 'end (format nil ">FINIS. (for ~a)" line)))))
         ((search "[[" line)
          (if (search "]]" line)
              (return (list 'comment line))
              (progn (setf *fountain-state* (cons :continued-note line))
                     (return nil))))
         ((or (starts-with-subseq "INT " line)
              (starts-with-subseq "EXT " line)
              (starts-with-subseq "INT. " line)
              (starts-with-subseq "EXT. " line))
          (let ((scene-name (mapcar (lambda (part)
                                      (pascal-case
                                       (string-trim #(#\Space #\Tab) part)))
                                    (split-sequence
                                     #\-
                                     (subseq line 4 (position #\# line))
                                     :count 2))))
            (with-simple-restart (check-for-scene-again "Check again for ~{~a~^/~}.tmx" scene-name)
              (unless (and (= 2 (length scene-name))
                           (probe-file (make-pathname
                                        :directory (list :relative "Source" "Maps" (first scene-name))
                                        :name (second scene-name)
                                        :type "tmx")))
                (error "Scene map file not found: Source/Maps/~{~a~^/~}.tmx ?"
                       scene-name)))
            (return (list 'scene scene-name))))
         ((every (lambda (char) (char= char (char-upcase char))) line)
          (destructuring-bind (kind name)
              (if (find #\( line)
                  (cl-ppcre:register-groups-bind (name parens)
                      ("^(.*?)[:space:]*\\((.+)\\)$" (string-trim #(#\Space #\Tab) line))
                    (assert (member parens '("OC" "O/C" "VO" "V/O")
                                    :test #'string-equal)
                            (parens)
                            "Parentheses after speaker's name ~
are only allowed to be used for off-camera (O/C) labels, but got “~a” in “~a”"
                            parens line)
                    (list 'speaker-oc (string-trim #(#\Space #\Tab) name)))
                  (list 'speaker line))
            (setf *fountain-state* (cons :speech ""))
            (return (list kind name))))
         (t (setf *fountain-state* (cons :stage line))
            (return nil))))
      (t (error "Unknown Fountain state ~s" *fountain-state*)))))

(defun format-terminals-for-error (expected-terminals)
  (mapcar (lambda (x)
            (if (null x) "nothing at all" (princ-to-string x)))
          (sort (copy-list expected-terminals) #'string<)))

(defmethod print-object ((condition yacc:yacc-parse-error) stream)
  "Print a YACC:YACC-PARSE-ERROR nicely to STREAM"
  (let ((terminal (yacc:yacc-parse-error-terminal condition))
        (value (yacc:yacc-parse-error-value condition))
        (expected-terminals (yacc:yacc-parse-error-expected-terminals condition)))
    (when *line-number*
      (format stream "When reading line ~:d:~%~4t" *line-number*))
    (when *last-values*
      (format stream "After reading “~{~a~^ ~}”:~%~4t" *last-values*))
    (if terminal
        (format stream "Read a token of type “~a” (value “~a”) unexpectedly."
                terminal value)
        (format stream "Read nil (end of stage directions?) unexpectedly."))
    (let ((len (length (remove-if #'null expected-terminals))))
      (cond
        ((zerop len)
         (format stream " Was not expecting anything further."))
        ((= 1 len)
         (format stream " Only expected “~a.”"
                 (let ((s (princ-to-string (first expected-terminals))))
                   (if (= 1 (length s))
                       (format nil "~@c (~a)"
                               (first-elt s)
                               (sentence-case
                                (char-name (first-elt s))))
                       s))))
        ((= 2 len)
         (format stream " Expected either ~{“~a” or “~a.”~}"
                 (format-terminals-for-error expected-terminals)))
        ((< len 10)
         (format stream " Expected one of these:~{~%~10t • ~a~}"
                 (format-terminals-for-error expected-terminals)))
        (t
         (format stream " Expected one of these:~% ~{“~a~^,”~:_ ~}”"
                 (format-terminals-for-error expected-terminals)))))))

(defvar *line-number* nil)
(defvar *current-pathname* nil)
(defvar *last-values* nil)

(defun fountain-lexer (stream)
  "A lexer for Fountain script source in STREAM"
  (loop for line = (read-line stream nil nil)
        for values = (fountain-lexer/parse-line line)
        until values
        do (incf *line-number*)
        finally (return (values-list values))))

(defun presence (sequence)
  "Returns NIL if SEQUENCE is `EMPTYP', else returns SEQUENCE"
  (if (emptyp sequence) nil sequence))

(defun actor-name-char-p (char)
  "Returns generally true if it's possible for CHAR to be part of an actor name"
  (or (alpha-char-p char) (char= #\- char)))

(defun numeric-char-p (char)
  "Returns generally true if it's possible for CHAR to be part of a number. [0-9./]"
  (or (digit-char-p char) (char= #\. char) (char= #\/ char)))

(defun stage-direction-lexer (stream)
  "A lexer for stage directions in STREAM"
  (labels ((rewind-stream (&optional (n 1))
             (file-position stream (- (file-position stream) n)))
           (token-values (string)
             (cond
               ((emptyp string) (multiple-value-list (stage-direction-lexer stream)))
               ((member string +stage-direction-words+ :test #'string-equal)
                (list (intern (string-upcase string) #.*package*) string))
               ((and (every #'digit-char-p (subseq string 0 (1- (length string))))
                     (member (last-elt string) '(#\. #\/) :test #'char=))
                (rewind-stream 2)
                (list 'number (parse-number (subseq string 0 (1- (length string))))))
               ((every #'numeric-char-p string)
                (list 'number (parse-number string)))
               ((every #'actor-name-char-p string)
                (list 'actor string))
               ((and (char= #\$ (char string 0))
                     (every (lambda (ch) (digit-char-p ch 16)) string)
                     (= 5 (length string)))
                (list 'memory (parse-number (subseq string 1) :radix 16)))
               ((and (char= #\$ (char string 0))
                     (every #'actor-name-char-p (subseq string 1)))
                (list 'variable string))
               ((char= #\" (char string 0))
                (list 'quoted (pascal-case (subseq string 1))))
               (t (list 'string string)))))
    (let ((parsed (loop with word = ""
                        for char = (read-char stream nil nil)
                        unless char
                          return (unless (emptyp word)
                                   (token-values word))
                        do (cond
                             ((member char (list #\Space #\Tab #\Newline))
                              (return (prog1 (token-values word)
                                        (setf word ""))))
                             ((and (char= char #\$)
                                   (emptyp word))
                              (setf word "$"))
                             ((char= char #\$)
                              (error "$ must introduce a variable name"))
                             ((and (char= char #\")
                                   (emptyp word))
                              (return (token-values
                                       (apply #'concatenate 'string
                                              #(#\quotation_mark)
                                              (loop for qq = (read-char stream nil nil)
                                                    until (or (char= qq #\") (null qq))
                                                    collecting (string qq))))))
                             ((and (actor-name-char-p char)
                                   (or (every #'actor-name-char-p word)
                                       (and (char= #\$ (char word 0))
                                            (every #'actor-name-char-p (subseq word 1)))))
                              (setf word (concatenate 'string word (string char))))
                             ((and (numeric-char-p char)
                                   (every #'numeric-char-p word))
                              (if (digit-char-p char)
                                  (setf word (concatenate 'string word (string char)))
                                  (if (or (find #\. word) (find #\/ word))
                                      (progn (rewind-stream 1)
                                             (return (token-values word)))
                                      (setf word (concatenate 'string word (string char))))))
                             ((emptyp word)
                              (return (list (intern (string char) #.*package*) (string char))))
                             (t (rewind-stream)
                                (return (token-values word)))))))
      (when parsed
        (destructuring-bind (token value) parsed
          (when (or token value)
            (appendf *last-values*
                     (list
                      (if (string-equal (princ-to-string value)
                                        (princ-to-string token))
                          (princ-to-string token)
                          (format nil "~a (~a)" value token))))
            (when (< 10 (length *last-values*))
              (setf *last-values* (subseq *last-values* 1)))
            (return-from stage-direction-lexer (values token value))))))))

(defun make-fountain-lexer (stream)
  "Creates a `FOUNTAIN-LEXER' bound to STREAM"
  (lambda () (fountain-lexer stream)))

(defun prepare-dialogue (string)
  "Prepare STRING for encoding into Minifont for the game console"
  (let ((prepared
          (string-trim
           #(#\Space #\Tab)
           (cl-ppcre:regex-replace-all
            "\\\\[A-Za-z0-9]+"
            (cl-ppcre:regex-replace-all
             "~([a-z]+){([a-z]*)}"
             (cl-ppcre:regex-replace-all
              "’s"
              (cl-ppcre:regex-replace-all
               "fi"
               (cl-ppcre:regex-replace-all
                "li"
                (cl-ppcre:regex-replace-all
                 "’r"
                 (cl-ppcre:regex-replace-all
                  "’ll"
                  (cl-ppcre:regex-replace-all
                   "I’"
                   (cl-ppcre:regex-replace-all
                    "I’ll"
                    (cl-ppcre:regex-replace-all
                     "—"
                     (cl-ppcre:regex-replace-all
                      "[ \\t\\n]+"
                      (cl-ppcre:regex-replace-all
                       "(\\.\\.\\.+)"
                       (cl-ppcre:regex-replace-all
                        "(\\b[A-Za-z0-9-']+\\b *)\\[.*?\\]( *)"
                        (cl-ppcre:regex-replace-all
                         "\\'"
                         string
                         "’")
                        "\\1\\2")
                       "…")
                      " ")
                     "-")
                    "{i’ll}")
                   "{i’}")
                  "{’ll}")
                 "{’r}")
                "{li}")
               "{fi}")
              "{’s}")
             "\\1\\2")
            ""))))
    ;; For round-trip validation, treat pilcrow (¶) as a space in the minifont
    ;; domain. Pilcrow is used as paragraph markup and has no minifont glyph.
    (let* ((no~ (remove #\} (remove #\{ (remove #\~ (remove #\¶ (string-downcase prepared))))))
           (encoded (ignore-errors (unicode->minifont no~)))
           (back+forth (when encoded (ignore-errors (minifont->unicode encoded)))))
      (assert (and encoded back+forth (string-equal no~ back+forth))
              (prepared)
              "This text contains character(s) which cannot be displayed on ~
the game console:
The prepared text would be
“~a”,
which would be rendered from approximately
“~a”
as
“~a”"
              prepared
              no~
              (or back+forth "nil"))
      (assert (>= #xc0 (length (unicode->minifont no~))) (prepared)
              "This text is more than 192 characters in length and cannot be prepared.
“~a”"
              prepared))
    prepared))

    (assert (handler-case (prepare-dialogue "Para¶check")
    (error (c)
    (not (search "string contains a non-printable character" (princ-to-string c))))))

(defvar *atarivox-dictionary* nil
  "A cache for the AtariVox (SpeakJet) dictionary from Source/Tables/SpeakJet.dic")

(defun ensure-atarivox-dictionary ()
  "Ensure that *ATARIVOX-DICTIONARY* has been loaded

May call `LOAD-ATARIVOX-DICTIONARY' if not already cached"
  (or *atarivox-dictionary*
      (setf *atarivox-dictionary*
            (load-atarivox-dictionary))))

(define-constant +all-phonemes+
    '(
      "Pause0" "Pause1" "Pause2" "Pause3" "Pause4" "Pause5" "Pause6"
      "Fast" "Slow" "Stress" "Relax" "Wait" "Soft" "Volume" "Speed" "Pitch" "Bend" "PortCtr"
      "Port" "Repeat" "CallPhrase" "GotoPhrase" "Delay" "Reset"
      "IY" "IH" "EY" "EH" "AY" "AX" "UX" "OH" "AW" "OW" "UH" "UW"
      "MM" "NE" "NO" "NGE" "NGO" "LE" "LO" "WW"
      "RR" "IYRR" "EYRR" "AXRR" "AWRR" "OWRR"
      "EYIY" "OHIY" "OWIY" "OHIH" "IYEH" "EHLE" "IYUW" "AXUW" "IHWW" "AYWW" "OWWW"
      "JH" "VV" "ZZ" "ZH" "DH" "BE" "BO" "EB" "OB"
      "DE" "DO" "ED" "OD" "GE" "GO" "EG" "OG"
      "CH" "HE" "HO" "WH" "FF" "SE" "SO" "SH" "TH"
      "TT" "TU" "TS" "KE" "KO" "EK" "OK" "PE" "PO"
      "R0" "R1" "R2" "R3" "R4" "R5" "R6" "R7" "R8" "R9"
      "A0" "A1" "A2" "A3" "A4" "A5" "A6" "A7" "A8" "A9"
      "B0" "B1" "B2" "B3" "B4" "B5" "B6" "B7" "B8" "B9"
      "C0" "C1" "C2" "C3" "C4" "C5" "C6" "C7" "C8" "C9"
      "DTMF_0" "DTMF_1" "DTMF_2" "DTMF_3" "DTMF_4" "DTMF_5"
      "DTMF_6" "DTMF_7" "DTMF_8" "DTMF_9" "DTMF_STAR" "DTMF_HASH"
      "M0" "M1" "M2" "EndOfPhrase"
      "PlayerNamePlace" "PlayerVerbS"
      "PronounThey" "PronounThem" "PronounTheir" "PronounTheirs"
      "PronounSibling" "PronounPerson" "PronounAre" "PronounWere" "PronounHave"
      "ButtonILabel" "ButtonIILabel" "ButtonIIILabel"
      "ButtonSelectLabel" "ButtonPauseLabel" "ButtonResetLabel"
      "PlayerHonorific" )
  :test #'equalp)

(defun phonemes-from-string (string)
  "Convert the AtariVox (SpeakJet) dictionary escape code phonemes in STRING into tokens"
  (when (and (not (eql :nil string))
             (not (string-equal "NIL" string)))
    (flatten
     (loop for phoneme in (split-sequence #\Space string
                                          :remove-empty-subseqs t)
           do (restart-case
                  (unless (char= #\\ (char phoneme 0))
                    (error "Expected SpeakJet phoneme code, beginning with \\, but got ~s" phoneme))
                (continue () :report "Continue with a gunshot sound"
                  "M1"))
           do (restart-case
                  (unless (or (numberp (ignore-errors (parse-number (subseq phoneme 1))))
                              (member (subseq phoneme 1) +all-phonemes+ :test #'string=))
                    (error "Expected SpeakJet phoneme code, but got: ~a" phoneme))
                (continue () :report "Continue with a gunshot sound"
                  "M1"))
           collect (if (every #'digit-char-p (subseq phoneme 1))
		   (format nil "$~2,'0x"
			 (logand #xff (parse-number (subseq phoneme 1))))
		   (subseq phoneme 1))))))

(defun load-atarivox-dictionary ()
  "Load the AtariVox (SpeakJet) dictionary from Source/Tables/SpeakJet.dic"
  (tagbody
   top
     (with-input-from-file (speakjet.dic #p"Source/Tables/SpeakJet.dic")
       (assert (equalp "[words]" (read-line speakjet.dic nil nil)) ()
               "SpeakJet.dic must begin with [words] magic cookie")

       (format *trace-output* "~&Reading SpeakJet.dic file … ")
       (restart-case
           (loop with dict = (make-hash-table :test 'equalp)
                 for line = (read-line speakjet.dic nil nil)
                 while line
                 do (unless (or (emptyp line) (char= #\# (char line 0)))
                      (destructuring-bind (word phonemes$)
                          (mapcar (lambda (bit) (string-trim #(#\Space #\Tab) bit))
                                  (split-sequence #\= line))
                        (when-let (orig (gethash word dict))
                          (error "Word defined twice in phonetic dictionary.
“~a” was previously ~s
but now also ~s."
                                 word orig (phonemes-from-string phonemes$)))
                        (when (string-equal "nil" phonemes$)
                          (setf (gethash word dict) :nil))
                        (setf (gethash word dict)
                              (phonemes-from-string phonemes$))))
                 finally (progn
                           (format *trace-output* " Done, ~:d word~:p"
                                   (hash-table-count dict))
                           (return-from load-atarivox-dictionary dict)))
         (reload-dictionary ()
           :report "Reload the AtariVox (SpeakJet) dictionary"
           (go top))))))

(defun reload-atarivox-dictionary ()
  "Reload the AtariVox (SpeakJet) dictionary into *ATARIVOX-DICTIONARY*"
  (setf *atarivox-dictionary* (load-atarivox-dictionary))
  (length (hash-table-keys *atarivox-dictionary*)))

(defun speakjet-pause+ (x y)
  "Sum together the SpeakJet pauses X and Y into one longer pause"
  (format nil "Pause~d"
          (+ (parse-integer x :start 5)
             (parse-integer y :start 5))))

(defun log-missing-word-for-speakjet (word)
  (with-output-to-file (missing-words #p"Object/SpeakJet.missing.words"
                                      :if-exists :append
                                      :if-does-not-exist :create)
    (fresh-line missing-words)
    (princ word missing-words)
    (fresh-line missing-words)))

(defun fixup-exclamations (seq)
  (loop
     (let ((bang (position-if (lambda (n) (member n '(:bang :query))) seq)))
       (unless bang
         (return-from fixup-exclamations seq))
       (assert (plusp bang) ()
               "Neither exclamation mark nor question mark can begin a sentence")
       (setf seq
             (let* ((alteration (elt seq bang))
                    (phrase-start
                      (or (let ((n (position-if
                                    (lambda (tok)
                                      (and (stringp tok)
                                           (starts-with-subseq "Pause" tok)))
                                    seq
                                    :end bang :from-end t)))
                            (when n (1+ n)))
                          0))
                    (before (subseq seq 0 phrase-start))
                    (phrase (subseq seq phrase-start bang))
                    (after (when (< bang (length seq))
                             (subseq seq (1+ bang))))
                    (phrase-length (length phrase)))
               (assert (plusp phrase-length) ()
                       "Neither exclamation mark nor question mark can modify a zero-phoneme-long phrase")
               (ecase alteration
                 (:bang
                  (warn "handling of “!” is poor")
                  (reduce (curry #'concatenate 'list)
                          (list
                           before
                           (list "Bend" "$04")
                           (mapcan (lambda (phoneme)
                                     (list "Stress" phoneme))
                                   phrase)
                           (list "Bend" "$05")
                           after)))
                 (:query
                  (warn "handling of “?” is poor")
                  (reduce (curry #'concatenate 'list)
                          (list
                           before
                           (case (length phrase)
                             (1 (list "Bend" "$08" (car phrase)))
                             (2 (list "Bend" "$06" (first phrase)
                                      "Bend" "$08" (second phrase)))
                             (3 (list "Bend" "$06" (first phrase)
                                      "Bend" "$08" (second phrase)
                                      "Bend" "$0a" (third phrase)))
                             (4 (list "Bend" "$06" (first phrase)
                                      "Bend" "$08" (second phrase)
                                      "Bend" "$0a" (third phrase)
                                      "Bend" "$08" (fourth phrase)))
                             (otherwise
                              (cons (subseq phrase 0 (- (length phrase) 5))
                                    (list "Bend" "$06" (elt phrase (- (length phrase) 5))
                                          "Bend" "$08" (elt phrase (- (length phrase) 4))
                                          "Bend" "$0a" (elt phrase (- (length phrase) 3))
                                          "Bend" "$0c" (elt phrase (- (length phrase) 2))
                                          "Bend" "$09" (elt phrase (- (length phrase) 1))))))
                           (list "Bend" "$05")
                           after)))))))))

(defmacro repeat-unrolled ((times) &body body)
  (cons 'progn
        (loop repeat times
              collect `(progn ,@ (copy-list body)))))

(defun combine-adjacent-pauses (bytes)
  (when (< (length bytes) 2)
    (return-from combine-adjacent-pauses bytes))
  (let ((merge1
          (append
           (loop for i from 0 below (1- (length bytes))
                 for a = (elt bytes i)
                 for b = (elt bytes (1+ i))
                 if (and (stringp a)
                         (stringp b)
                         (starts-with-subseq "Pause" a)
                         (starts-with-subseq "Pause" b))
                   collect (prog1 (speakjet-pause+ a b)
                             (incf i))
                 else
                   if (and (stringp a)
                           (member b '(:bang :query))
                           (starts-with-subseq "Pause" a))
                     collect (prog1 b
                               (incf i))
                 else
                   if (and (stringp a)
                           (starts-with-subseq "Pause" a)
                           (string= b "EndOfPhrase"))
                     collect (prog1 b
                               (incf i))
                 else
                   collect a)
           (last bytes))))
    (let ((penultimate (elt merge1 (- (length merge1) 2)))
          (ultimate (elt merge1 (- (length merge1) 1))))
      (if (and (stringp penultimate)
               (stringp ultimate)
               (starts-with-subseq "Pause" penultimate)
               (or (starts-with-subseq "Pause" ultimate)
                   (string= "EndOfPhrase" ultimate)))
          (return-from combine-adjacent-pauses
            (combine-adjacent-pauses
             (append (subseq merge1 0 (- (length merge1) 1))
                     (list "EndOfPhrase"))))
          merge1))))

(defun char-digit-or-comma-p (char)
  (or (digit-char-p char) (char= #\, char)))

(defun convert-for-atarivox (string)
  "Convert STRING into a list of tokens for AtariVox (SpeakJet)"
  (ensure-atarivox-dictionary)
  (when (emptyp string) (return-from convert-for-atarivox nil))
  (let ((string (cl-ppcre:regex-replace-all
                 "\\b[\\p{L}\\p{N}’'-]+\\s*\\[(.*?)\\]"
                 string
                 " \\1 "))
        (words nil))
    (cl-ppcre:do-scans (start end reg-starts reg-ends
                        "(\\s+|-|\\\\\\d+|[~\\\\]\\p{L}+|[\\p{L}\\p{N}’']+|[^\\s\\p{L}\\p{N}’'-]+)" string)
      (let ((word (string-trim #(#\Space #\Tab #\Newline)
                               (subseq string start end) )))
        (push word words)))
    (let ((output (list)))
      (dolist (word words)
        (if (and (not (emptyp word))
                 (some #'digit-char-p word)
                 (every #'char-digit-or-comma-p word))
            (dolist (num (reverse (split-sequence-if-not #'alpha-char-p
                                                         (format nil "~r" (parse-number (remove #\, word))))))
              (push num output))
            (push word output)))
      (setf words output))
    (let ((bytes (loop
                   for word in words
                   append (cond
                            ((emptyp word) (list "Pause1"))
                            ((char= (char word 0) #\\)
                             (if (every #'digit-char-p (subseq word 1))
                                 (list (format nil "$~2,'0x" (parse-number (subseq word 1))))
                                 (list (subseq word 1))))
                            ((equalp word "?!") (list :bang :query))
                            ((equalp word "!") (list :bang))
                            ((equalp word "?") (list :query))
                            ((member word '("-" "“" "”") :test #'string-equal)
                             nil)
                            ((or (eql :nil (gethash word *atarivox-dictionary*))
                                 (null (gethash word *atarivox-dictionary* '#:nothing-was-there)))
                             nil)
                            (t (or (gethash word *atarivox-dictionary*)
                                   (progn
                                     (log-missing-word-for-speakjet word)
                                     (cerror "Continue with a gunshot sound"
                                             "Word not in dictionary: “~a” not found"
                                             word)
                                     (list "M1"))))))))
      (flatten
       (append (remove-if #'null
                          (fixup-exclamations (combine-adjacent-pauses bytes)))
               (cons "EndOfPhrase" nil))))))

(defun compile-fountain-script (pathname)
  "Compile the Fountain script in PATHNAME into source code (to *STANDARD-OUTPUT*)"
  (with-input-from-file (fountain pathname)
    (format t "~% ( Compiled from ~a ) " pathname)
    (let ((*current-pathname* pathname))
      (compile-fountain-stream fountain))))

(defgeneric npc-interpret-field (value field &key name kind)
  (:method (value (field t) &key name kind)
    (declare (ignore name kind))
    (warn "Using default NPC-INTERPRET-FIELD for ~s" field)
    (etypecase value
      (string (parse-number value))
      (number value)
      (null nil))))

(defun npc-interpret-color (moniker)
  (cond
    ((emptyp moniker)
     (cerror "Use WHITE for now" "Missing a color value")
     "PaletteColor_White")
    ((member moniker *common-palette*
             :test #'string-equal)
     (format nil "PaletteColor_~:(~a~)" moniker))
    (t
     (cerror "Use WHITE for now" "Unrecognized color value “~a”" moniker)
     "PaletteColor_White")))

(defmethod npc-interpret-field (moniker (field (eql :hair-color)) &key name kind)
  (declare (ignore name kind))
  (when moniker (npc-interpret-color moniker)))

(defmethod npc-interpret-field (moniker (field (eql :skin-color)) &key name kind)
  (declare (ignore name kind))
  (when moniker (npc-interpret-color moniker)))

(defmethod npc-interpret-field (moniker (field (eql :clothes-color)) &key name kind)
  (declare (ignore name kind))
  (when moniker (npc-interpret-color moniker)))

(defmethod npc-interpret-field (chalice (field (eql :chalice)) &key kind name)
  (declare (ignore name kind))
  (if (emptyp chalice)
      0
      (parse-integer chalice)))

(defmethod npc-interpret-field (head (field (eql :head)) &key kind name)
  (when (or (null kind) (eql kind 'human))
    (if (emptyp head)
        (progn (cerror "Use head 0"
                       "Head number must be 0-9~@[ for actor ~:(~a~)~]" name)
               0)
        (parse-integer head))))

(defmethod npc-interpret-field (body (field (eql :body)) &key kind name)
  (cond
    ((and (eql kind 'sailor)
          (let ((n (ignore-errors (parse-number body))))
            (or (null n) (not (<= 0 n 8)))))
     (cerror "Continue with sailor “0”"
             "Sailor body should be 0-8, but got “~a”~@[ for actor ~:(~a~)~]"
             body name)
     0)
    ((eql kind 'sailor)
     (parse-number body))
    ((not (eql kind 'human))
     0)
    ((emptyp body)
     (cerror "Use tunic body"
             "Body type should be “tunic” or “robe” but got “~a”~@[ for actor ~:(~a~)~]"
             body name)
     0)
    ((string-equal "tunic" body) 0)
    ((string-equal "robe" body) 1)
    (t (cerror "Use tunic body"
               "Body type should be “tunic” or “robe” but got “~a”~@[ for actor ~:(~a~)~]"
               body name)
       0)))

(defmethod npc-interpret-field (speech-color (field (eql :speech-color)) &key name kind)
  (declare (ignore kind))
  (if (emptyp speech-color)
      (progn
        (cerror "Use gray"
                "Speech balloon color not specified~@[ for actor ~:(~a~)~]" name)
        "GRAY")
      speech-color))

(defmethod npc-interpret-field (voice-pitch (field (eql :voice-pitch)) &key name kind)
  (declare (ignore kind))
  (if (emptyp voice-pitch)
      (progn
        (cerror "Use generic male voice pitch 96"
                "Voice pitch not specified~@[ for actor ~:(~a~)~]" name)
        96)
      (parse-integer voice-pitch)))

(defmethod npc-interpret-field (voice-speed (field (eql :voice-speed)) &key name kind)
  (declare (ignore kind))
  (or (unless (emptyp voice-speed)
        (parse-integer voice-speed))
      (progn
        (cerror "Use default voice speed 114"
                "Voice speed not specified~@[ for actor ~:(~a~)~]" name)
        114)))

(defmethod npc-interpret-field (voice-bend (field (eql :voice-bend)) &key name kind)
  (declare (ignore kind))
  (or (unless (emptyp voice-bend)
        (parse-integer voice-bend))
      (progn
        (cerror "Use default voice bend 5"
                "Voice bend not specified~@[ for actor ~:(~a~)~]" name)
        5)))

(defmethod npc-interpret-field (crowns (field (eql :crowns)) &key name kind)
  (declare (ignore name kind))
  (if (emptyp crowns) 0
      (abs (parse-integer crowns))))

(defmethod npc-interpret-field (arrows (field (eql :arrows)) &key name kind)
  (declare (ignore name kind))
  (if (emptyp arrows) 0
      (abs (parse-integer arrows))))

(defmethod npc-interpret-field (potions (field (eql :potions)) &key name kind)
  (declare (ignore name kind))
  (if (emptyp potions) 0
      (abs (parse-integer potions))))

(defmethod npc-interpret-field (gender (field (eql :gender)) &key name kind)
  (declare (ignore name kind))
  (if (emptyp gender) "Nonbinary"
      (ecase (make-keyword (string-upcase (char gender 0)))
        (:m "Male")
        (:f "Female")
        ((:x :n) "Nonbinary"))))

(defun load-actor (actor)
  (tagbody top
     (restart-case
         (destructuring-bind  (&key name decal body head hair skin clothing
                                    voice-pitch voice-speed voice-bend speech-color
                                    hp ac character-id nicks class gender
                                    crowns arrows potions chalice
                               &allow-other-keys)
             (find-npc-stats actor)
           (unless character-id
             (cerror "Continue, using Norville"
                     "Actor ~:(~a~) could not be found in NPC stats file" actor)
             (return-from load-actor (load-actor "Norville")))
           (let* ((kind (cond ((emptyp decal)
                               (cerror "Continue, using HUMAN as the decal kind"
                                       "Decal kind not given for “~:(~a~)”/“~a”"
                                       actor name))
                              ((member decal '("human" "Vizier" "Nefertem"
                                               "Earl" "Captain" "Princess" "Elder" "sentinel" "sailor")
                                       :test #'string-equal)
                               (intern (string-upcase decal) #.*package*))
                              (t (cerror "Use generic HUMAN decal"
                                         "Decal kind “~a” not recognized (for “~:(~a~)”/“~a” in NPC stats)"
                                         decal actor name))))
                  (record
                    (append (list :name name
                                  :kind kind
                                  :hp (unless (emptyp hp) (parse-integer hp))
                                  :ac (unless (emptyp ac) (parse-integer ac))
                                  :pitch (npc-interpret-field voice-pitch :voice-pitch :name name)
                                  :speed (npc-interpret-field voice-speed :voice-speed :name name)
                                  :bend (npc-interpret-field voice-bend :voice-bend :name name)
                                  :speech-color (npc-interpret-field speech-color
                                                                     :speech-color :name name)
                                  :character-id character-id
                                  :nicks nicks
                                  :class class
                                  :gender (npc-interpret-field gender :gender :name name)
                                  :crowns (npc-interpret-field crowns :crowns :name name)
                                  :arrows (npc-interpret-field arrows :arrows :name name)
                                  :potions (npc-interpret-field potions :potions :name name)
                                  :chalice (npc-interpret-field chalice :chalice :name name)
                                  :hair-color (npc-interpret-field hair :hair-color :name name)
                                  :skin-color (npc-interpret-field skin :skin-color :name name)
                                  :clothes-color (npc-interpret-field clothing :clothes-color
                                                                      :name name))
                            (when (eql kind 'sailor)
                              (list :body (npc-interpret-field body :body :kind kind :name name)))
                            (when (eql kind 'human)
                              (list :head (npc-interpret-field head :head :kind kind :name name)
                                    :body (npc-interpret-field body :body :kind kind :name name))))))
             (if-let (i (position-if
                         (lambda (actor)
                           (and (consp actor)
                                (< 1 (length actor))
                                (getf actor :character-id)
                                (= character-id (getf actor :character-id))))
                         *actors*))
               (setf (elt *actors* i) record)
               (push record *actors*))
             (return-from load-actor record))))))

(defun find-location (location)
  "Given an Entrance name LOCATION, return (LIST X Y)"
  (unless *current-scene*
    (error "No scene is started; cannot find a location in the void (Looking for ~s)"
           location))
  (destructuring-bind  (locale-id x y) (find-entrance-by-name
                                        (load-other-map *current-scene*)
                                        *current-scene* location)
    (declare (ignore locale-id))
    (list x y)))

(defvar *current-scene* nil)
(defvar *actors* nil)
(defvar *deferred-weather* nil
  "Holds weather-related stage directions or data that are deferred during scene preparation.
Expected format: a list of weather stage direction forms or data structures to be processed later.
Lifecycle: Set to NIL at the start of scene preparation, populated as weather directives are encountered,
and processed/applied at the appropriate point in the scene setup.")

(defvar *deferred-weather-lock* (sb-thread:make-mutex :name "deferred-weather-lock")
  "Mutex for protecting access to *deferred-weather* during concurrent operations.")

(defgeneric compile-stage-direction (fun args)
  (:method ((fun t) (args t))
    (error "Unhandled stage direction function ~s" fun)))

(defmacro defstage (fun (&rest args) &body body)
  (let ((argv (gensym "ARGS-")))
    `(defmethod compile-stage-direction ((f (eql ',fun)) (,argv cons))
       (block nil
         (destructuring-bind (,@args) ,argv
           ,@body)))))

(defun interpret-place (place)
  (destructuring-bind (kind entrance-name &optional n1 d1 n2 d2) place
    (ecase kind
      (absolute (destructuring-bind (kind x y) place
                  (declare (ignore kind))
                  (list :absolute x y)))
      (place (destructuring-bind (locale-id x y) (find-entrance-by-name
                                                  (locale-xml *current-scene*)
                                                  entrance-name
                                                  *current-scene*)
               (declare (ignore locale-id))
               (ecase d1
                 (north (decf y n1))
                 (south (incf y n1))
                 (east (incf x n1))
                 (west (decf x n1))
                 ((nil) nil))
               (ecase d2
                 (north (decf y n2))
                 (south (incf y n2))
                 (east (incf x n2))
                 (west (decf x n2))
                 ((nil) nil))
               (list :absolute x y)))
      (δ (destructuring-bind (δ n1 d1 &optional n2 d2) place
           (declare (ignore δ))
           (let ((δx 0) (δy 0))
             (ecase d1
               (north (decf δy n1))
               (south (incf δy n1))
               (east (incf δx n1))
               (west (decf δx n1))
               ((nil) nil))
             (ecase d2
               (north (decf δy n2))
               (south (incf δy n2))
               (east (incf δx n2))
               (west (decf δx n2))
               ((nil) nil))
             (list :relative δx δy)))))))

(defun color-index (name)
  (or (when (null name) nil)
      (position name *common-palette* :test #'string-equal)
      (error "Not a common color name: ~a (expected one of: ~s)" name *common-palette*)))

(defstage pick-up (actor item-name)
  (declare (ignore actor))
  (destructuring-bind (&key x y gid) (find-named-object-in-scene item-name)
    (if (and x y gid)
        (let ((x (floor (the real x) 8))
              (y (1- (floor (the real y) 16)))
              (art (logand #xff (* 2 (1- (the real gid))))))
          (format t "~% ~d ~d  ( ~a = ) ~d destroy-decal"
                  x y item-name art))
        (format t "~% ( garbage request to pick-up ignored, perhaps nothing named ~s was found ) " item-name))))

(defstage progn (&rest directions)
  (map nil #'stage-directions->code directions))

(defstage music (start/stop song)
  (ecase start/stop
    (start (format t "
Song_~a_ID NextSong C!
SoundSourceBackgroundMusic NextSoundSource C!
PlaySong EXECUTE " song))
    (incidental (format t "
Song_~a_ID NextSong C!
SoundSourceIncidentalMusic NextSoundSource C!
PlaySong EXECUTE "  song))))

(defstage hurt (actor amount)
  (destructuring-bind (&key name &allow-other-keys)
      (require-actor actor)
    (format t "~%~d CharacterID_~a hurt-actor "
            (round amount)
            (pascal-case (string name)))))

(defstage equip (actor item-ident)
  (destructuring-bind (&key name &allow-other-keys)
      (require-actor actor)
    (format t "~%CharacterID_~a " (pascal-case (string name))))
  (let ((item (format nil "~{~a~^ ~}" (ensure-list item-ident))))
    (if (search "SHIELD" (string-upcase item))
        (format t " find-character CharacterShield Shield~a prop!"
                (pascal-case (string item)))
        (format t " find-character CharacterEquipment Equip~a prop!"
                (pascal-case (string item))))))


(defstage prepare (&rest directions)
  (format t "~%prepare-scene ")
  (map nil #'stage-directions->code directions)
  ;; Emit any deferred weather commands just before scene-ready
  (sb-thread:with-mutex (*deferred-weather-lock*)
    (dolist (weather-command (reverse *deferred-weather*))
      (format t "~% Weather~:(~a~) weather!"
              (pascal-case (string (or (first weather-command) "None")))))
    (setf *deferred-weather* nil))
  (format t " scene-ready~%"))

(defstage lighting-change (target &optional (speed 'normal))
  (format t "~% Lighting~a FadeSpeed~a change-lighting"
          (pascal-case (string target))
          (pascal-case (string speed))))

(defstage lighting (target)
  (format t "~% Lighting~a lighting!"
          (pascal-case (string target))))

(defstage sleep (actor)
  (destructuring-bind (&key name found-in-scene-p &allow-other-keys)
      (require-actor actor)
    (unless found-in-scene-p
      (cerror "Continue, ignoring sleep request"
              "Actor ~:(~a~) asked to go to sleep, but they are not in the scene" actor)
      (return))
    (let ((ok-label (genlabel "FoundCharacter"))
          (done-label (genlabel "DoneSleep")))
      (format t "~% ActionSleep CharacterID_~a character-action!"
              (pascal-case (string name))))))

(defun perform-character-action (actor action-verb action-enum)
  "Base function for character actions that sets the action enum and generates character-action! call"
  (destructuring-bind (&key name found-in-scene-p &allow-other-keys)
      (require-actor actor)
    (unless found-in-scene-p
      (cerror "Continue, ignoring ~a request"
              "Actor ~:(~a~) asked to ~a, but they are not in the scene" actor action-verb)
      (return-from perform-character-action))
    (let ((ok-label (genlabel (string-capitalize action-verb)))
          (done-label (genlabel (format nil "Done~a" (string-capitalize action-verb)))))
      (format t "~% CharacterID_~a ~a character-action!"
              (pascal-case (string name))
              action-enum))))

(defstage dance (actor)
  "ACTOR should perform the “dance” action."
  (perform-character-action actor "dance" "ActionDance"))

(defstage gesture (actor)
  "ACTOR should perform the “gesture” action."
  (perform-character-action actor "gesture" "ActionGesture"))

(defstage panic (actor)
  "ACTOR should perform the ”panic” action."
  (perform-character-action actor "panic" "ActionPanic"))

(defstage fly (actor)
  "ACTOR should perform the “fly” action."
  (perform-character-action actor "fly" "ActionFlying"))

(defstage wave-arms (actor)
  "ACTOR should perform the “wave arms” action."
  (perform-character-action actor "wave arms" "ActionWaveArms"))

(defstage weather (&optional kind)
  ;; Store the weather command to be emitted after load-map
  (push (list kind) *deferred-weather*))

(defstage wake (actor)
  "ACTOR should stop their current action and return to idle state."
  (destructuring-bind (&key name found-in-scene-p &allow-other-keys)
      (require-actor actor)
    (unless found-in-scene-p
      (cerror "Continue, ignoring awakening request"
              "Actor ~:(~a~) asked to awaken, but they are not in the scene" actor)
      (return))
    (let ((ok-label (genlabel "WakeUp"))
          (done-label (genlabel "DoneWaking")))
      (format t "~% ActionIdle CharacterID_~a character-action!"
              (pascal-case (string name))))))

(defstage fade-in (from-color &optional (speed 'normal))
  (format t "~% FadeSpeed~a FadeActualColor~:(~a~) fade-in"
          (pascal-case (string speed))
          (pascal-case (string from-color))))

(defstage fade-out (to-color &optional (speed 'normal))
  (format t "~% FadeSpeed~a FadeColor~a fade-out"
          (pascal-case (string speed))
          (pascal-case (string to-color))))

(defstage camera-center (where)
  (destructuring-bind (abs/rel x y) (interpret-place where)
    (assert (eql abs/rel :absolute))
    (format t "~% ~d ~d center-camera" x y)))

(defstage jump (script-id)
  (format t "~% ~d ( ~:*$~4,'0x ) load-script" script-id))

(defstage wait (beat/second duration)
  (let* ((secs (ecase beat/second
                 (beat (/ duration 2))
                 (second duration))))
    (if (< secs 255/50)
        (format t "~% ~d wait-deciseconds"
                (round (* 10 secs)))
        (format t "~% ~d wait-long"
                (floor (* 10 secs) 256)))))

(defstage wait-for (actor)
  (destructuring-bind (&key name found-in-scene-p &allow-other-keys)
      (require-actor actor)
    (unless found-in-scene-p
      (cerror "Continue, ignoring wait request"
              "Asked to wait for actor ~:(~a~), but they are not in the scene" actor)
      (return))
    (format t "~%CharacterID_~a settle-actor" (pascal-case (string name)))))

(defvar *dict-words* nil)

(defun read-entire-dictionary (&optional (dict-file #p"/usr/share/dict/words"))
  (or *dict-words*
      (let ((dict (make-hash-table :test 'equal)))
        (with-input-from-file (file dict-file)
          (loop for line = (read-line file nil nil)
                while line
                do (setf (gethash line dict) t)))
        (setf *dict-words* dict))))

(defun genlabel (prefix)
  (let* ((hash (format nil "~36r" (sxhash (string (gensym prefix)))))
         (l (min (length hash) 6)))
    (format nil "~a_~a"
            prefix
            (subseq hash (- (length hash) l) (length hash)))))

(defstage exit (who)
  (let ((not-found-character-label
          (genlabel "CharacterNotFound")))
    (destructuring-bind (&key name found-in-scene-p &allow-other-keys)
        (require-actor who)
      (unless found-in-scene-p
        (cerror "Continue, ignoring “exit” direction"
                "Asked for ~:(~a~) to exit the scene, which they were not in" name)
        (return))
      (format t "~% CharacterID_~a exit-character"
              (pascal-case (string name))))))

(defstage enter (who where)
  (destructuring-bind (actor found-in-scene-p) (find-or-load-actor who)
    (unless found-in-scene-p
      (push actor *actors*))
    #+ () (format *trace-output* "~&//* Character ~a entering scene ~@[… but they were already here~]"
                  (sentence-case (getf actor :name)) found-in-scene-p)
    (if (eql :oc where)
        (destructuring-bind (&key name &allow-other-keys) actor
          (if found-in-scene-p
              (format t "~% CharacterID_~a find-character entity-decal@ DUP
  127 SWAP DecalXH SWAP decal!
  127 SWAP DecalYH SWAP decal!"
                      (pascal-case (string name)))
              ;; else not found in scene
              (format t "~% CharacterID_~a find-character-in-scene entity-decal@ DUP
  127 SWAP DecalXH SWAP decal!
  127 SWAP DecalYH SWAP decal!"
                      (pascal-case (string name)))))
        ;; else, on camera
        (destructuring-bind (abs/rel x y) (interpret-place where)
          (assert (eql abs/rel :absolute))
          (destructuring-bind (&key name &allow-other-keys) actor
            (if found-in-scene-p
                (format t "~% CharacterID_~a find-character-in-scene entity-decal@ DUP DUP
  ~d SWAP DecalXH SWAP decal!
  ~d SWAP DecalYH SWAP decal!
  update-one-decal"
                        (pascal-case (string name)) x y)
                ;; else not already in scene
                (format t "~% ~d ~d CharacterID_~a enter-character"
                        x y (pascal-case (string name)))))))))

(defvar *boat-ids* nil)
(defvar *boat-classes* nil)

(defstage boat (ship-name east/west target actors)
  (declare (ignore actors)) ; TODO: #1239
  (with-simple-restart (reload-boats "Reload Boats.ods and retry")
    (load-boats)
    (let ((boat-id (gethash ship-name *boat-ids*))
          (boat-class (gethash ship-name *boat-classes*)))
      (format t "~%~10t( Boat “~:(~a~)” appears, headed to ~{~a~^ ~} )" ship-name target)
      (destructuring-bind (kind x y) (interpret-place target)
        (assert (eql kind :absolute) (kind)
                "KIND of location for positioning a boat must be absolute, but got ~s" kind)
        (format t "~%~d ( Boat “~a” ) DUP " boat-id ship-name)
        (format t "Boat~:(~a~) ~a"
                boat-class
                (ecase east/west
                  (at (format nil " ~d " x))
                  (east " MapWidth C@ ")
                  (west " -3 ")))
        (format t " ~d make-boat " y)
        ;; TODO: #1221 put people on the boat
        (when (eql east/west 'at)
          (return))
        (ecase east/west
          (east (format t " boat-sail-west "))
          (west (format t " boat-sail-east ")))
        (format t " ~d BoatDestX C! wait-for-boat-to-anchor"
                x)))))

(defstage sail-away (ship-name east/west)
  (with-simple-restart (reload-boats "Reload Boats.ods and retry")
    (load-boats)
    (let ((boat-id (gethash ship-name *boat-ids*)))
      (format t "~%~d ( Boat “~a” ) find-boat DUP " boat-id ship-name)
      (ecase east/west
        (east (format t " boat-sail-away-east"))
        (west (format t " boat-sail-away-west")))
      (format t " ~d ( “~a” ) wait-for-boat " boat-id ship-name))))

(defstage emote (actor emotion)
  (destructuring-bind (&key name &allow-other-keys) (require-actor actor)
    (destructuring-bind (&key position graphic class)
        (ecase emotion
          (sweat (list :position :over
                       :graphic "GrSweatEmote"
                       :class "SweatParticle"))
          (? (list :position :above
                   :graphic "GrQueryEmote"
                   :class "Pivitz"))
          (! (list :position :above
                   :graphic "GrBangEmote"
                   :class "Pivitz")))
      (format t "~% ~aClass ClassID C! ~:*~aSize Size C!
  CharacterID_~a CurrentCharacterID C!
  ~:[0~;1~] RelativePlacement C! ~a FillPattern C! Emote EXECUTE
  "
              class
              (pascal-case (string name))
              (eql :above position)
              graphic))))

(defstage embarks (actor ship-name)
  (format t "~% ( TODO actor ~a embarks upon ship ~a )" actor ship-name))

(defstage disembarks (actor ship-name)
  (format t "~% ( TODO actor ~a disembarks from ship ~a )" actor ship-name))

(defstage walk (who where &key (waitp t))
  (if (eql who 'player)
      (format t "~% ( TODO: Move the player )") ; TODO #151
      (destructuring-bind (&key name found-in-scene-p &allow-other-keys)
          (require-actor who)
        (destructuring-bind (abs/rel x y) (interpret-place where)
          (unless found-in-scene-p
            (if (eql abs/rel :absolute)
                (progn
                  (cerror "Continue, entering them at the destination"
                          "Actor ~:(~a) was not in scene when requested to walk to ~a"
                          who where)
                  (compile-stage-direction 'enter (list who where))
                  (return))
                (progn (cerror "Continue, ignoring the request"
                               "Actor ~a was not in scene when requested to walk relative to their current position"
                               who)
                       (return))))
          (format t "~% ~d ~d CharacterID_~a ~[ do-walk ~; do-walk-relative ~]"
  x y (pascal-case (string name))
  (ecase abs/rel
    (:absolute 0)
    (:relative 1)))
          (when waitp
            (format t "~% CharacterID_~a settle-actor"
                    (pascal-case (string name))))))))

(defstage look (who how)
  (loop for i from 0
        for npc in *npc-stats*
        when (string-equal (getf npc :name) who)
          do
             (loop for fact in how
                   do
                      (destructuring-bind (key value) fact
                        (ecase key
                          (skin (setf (getf npc :skin)
                                      (npc-interpret-field value :skin-color :name who)))
                          (hair (setf (getf npc :hair)
                                      (npc-interpret-field value :hair-color :name who)))
                          (tunic (setf (getf npc :clothing)
                                       (npc-interpret-field value :clothes-color :name who)
                                       (getf npc :body) 0))
                          (robe (setf (getf npc :clothing)
                                      (npc-interpret-field value :clothes-color :name who)
                                      (getf npc :body) 1))
                          (head (setf (getf npc :head)
                                      (let ((n (parse-integer value)))
                                        (check-type n (integer 0 9)
                                                    "the number of a head (0-9)")
                                        n)))))
                      (return how))))

(defun stage-facing-value (direction &key (playerp nil))
  (declare (ignore playerp))
  (ecase direction
    (north "ActorFacingUp")
    (south "ActorFacingDown")
    (east "ActorFacingRight")
    (west "ActorFacingLeft")))

(defstage face (who where)
  (destructuring-bind (&key name found-in-scene-p &allow-other-keys)
      (require-actor who)
    (unless found-in-scene-p
      (cerror "Continue, ignoring facing request"
              "Actor ~:(~a~) asked to face a direction, but they are not in the scene" who)
      (return))
    (format t "~% ~a CharacterID_~a character-facing!"
            (stage-facing-value where)
            (pascal-case (string name)))))

(defgeneric compile-stage-math (fun args)
  (:method ((fun t) (args t))
    (error "Unimplemented math function ~s" fun)))

(defmacro defmath (fun (&rest args) &body body)
  `(defmethod compile-stage-math ((f (eql ',fun)) args)
     (block nil
       (destructuring-bind (,@args) args
         ,@body))))

(defun forth-number (n)
  (format nil " ~d " n))

(defmacro define-simple-math ((fun) &body forth)
  (let ((forth* (gensym "FORTH-")))
    `(defmath ,fun (a b)
       (let ((a* (stage/constant-value a))
             (b* (stage/constant-value b))
             (,forth* ',forth))
         (cond
           ((and a* b*)
            (return (,fun a* b*)))
           (a*
            (stage-directions->acc b)
            (format t forth (forth-number  a*)))
           (b*
            (stage-directions->acc a)
            (format t forth (forth-number b*)))
           (t
            (stage-directions->acc a)
            (stage-directions->acc b)
            (princ forth)))))))

(define-simple-math (+) "+")
(define-simple-math (-) "-")
(define-simple-math (logand) "and")
(define-simple-math (logior) "or")
(define-simple-math (logxor) "xor")

(defgeneric compile-time-math (fun args)
  (:method ((fun t) (args t))
    (error "No compile-time math function for ~s ~s" fun args)))

(defmacro define-compiler-math (fun (&rest args) &body body)
  (let ((args* (loop for arg in args
                     if (char= #\& (char (string arg) 0))
                       collect arg
                     else
                       collect (intern (format nil "~a*" arg) #.*package*)))
        (argv (gensym "ARGS-")))
    `(defmethod compile-time-math ((fun (eql ',fun)) (,argv cons))
       (destructuring-bind (,@args*) ,argv
         (let (,@(loop for arg in args
                       for arg* in args*
                       unless (char= #\& (char (string arg) 0))
                         collect (list arg (list 'stage/constant-value arg*))))
           (if (some #'null (list ,@(remove-if (lambda (arg) (char= #\& (char (string arg) 0)))
                                               args)))
               (error "Compile-time maths require constant expressions, got ~s"
                      ,argv)
               (progn ,@body)))))))

(define-compiler-math + (a b) (+ a b))
(define-compiler-math - (a &optional b) (if b (- a b) (- a)))
(define-compiler-math × (a b) (* a b))
(define-compiler-math ÷ (a b) (/ a b))
(define-compiler-math √ (a) (sqrt a))
(define-compiler-math expt (a b) (expt a b))
(define-compiler-math log (a b) (log a b))

(defun stage/constant-value (expression)
  (block constant
    (etypecase expression
      (number (values expression t))
      (cons
       (let ((args (mapcar #'stage/constant-value (cdr expression))))
         (when (some #'null args)
           (return-from constant (values nil nil)))
         (values (compile-time-math (car expression)
                                    (cdr expression))
                 t)))
      (symbol (values nil nil))
      (string (values nil nil)))))

(defun script-auto-label (&optional (prefix "ScriptAutoLabel"))
  (string (genlabel prefix)))

(defgeneric compile-stage-directions-test (fun args)
  (:method ((fun t) (args t))
    (error "No comparison/test for ~s ~s" fun args)))

(defmacro defcomparison (fun (&rest args) &body body)
  `(defmethod compile-stage-directions-test ((fun (eql ',fun)) args)
     (destructuring-bind (,@args) args
       (block nil ,@body))))

(defun stage-directions->acc (expr)
  (etypecase expr
    (number
     (check-type expr (integer 0 #xffff))
     (format t " ~d ( ~:*$~4,'0x )" expr))
    (cons
     (let ((const (stage/constant-value expr)))
       (cond
         ((and const
               (typep const '(integer 0 #xffff)))
          (format t " ~d ( ~:*~4,'0x ) " const))
         (const
          (error "Cannot fit the value ~s (from ~s) into a word"
                 const expr))
         ((eql 'var (car expr))
          (format t " ~a " (field->label expr)))
         (t (error "Unknown expression kind: ~s" expr)))))))

(defun stage/constant-zero-p (x)
  (let ((n (stage/constant-value x)))
    (and n (zerop n))))

(defcomparison = (a b)
  (cond
    ((stage/constant-zero-p a)
     (stage-directions->acc b)
     (return "beq"))
    ((stage/constant-zero-p b)
     (stage-directions->acc a)
     (return "beq"))
    (t (stage-directions->acc a)
       (format t "~10tsta ScriptTemp0")
       (stage-directions->acc b)
       (format t "~10tcmp ScriptTemp0")
       (return "beq"))))

(defun invert-branch (branch)
  (when-let (inverted (cdr (assoc branch
                                  '((beq bne) (bne beq)
                                    (bpl bmi) (bmi bpl)
                                    (bvc bvs) (bvs bvc)
                                    (bcc bcs) (bcs bcc))
                                  :test #'string-equal)))
    (string-downcase (car inverted))))

(defun stage-directions->test (expr true false)
  (cond
    ((and (null true) (null false)) nil)
    ((null false)
     (let ((label (script-auto-label "ScriptTest"))
           (branch (invert-branch (compile-stage-directions-test
                                   (car expr) (cdr expr)))))
       (format t "~%~10t~a ~a~%" branch label)
       (stage-directions->code true)
       (format t "~%~a:" label)))
    ((null true)
     (let ((label (script-auto-label "ScriptBranchIfTrue"))
           (branch (compile-stage-directions-test (car expr) (cdr expr))))
       (format t "~%~10t~a ~a" branch label)
       (stage-directions->code false)
       (format t "~%~a:" label)))
    (t (let ((label1 (script-auto-label "ScriptBranchIfTrue"))
             (label2 (script-auto-label "ScriptBranchDone"))
             (branch (compile-stage-directions-test (car expr) (cdr expr))))
         (format t "~%~10t~a ~a~%" branch label1)
         (stage-directions->code false)
         (format t "~%~10tjmp ~a~%" label2)
         (format t "~2%~a:" label1)
         (when true
           (stage-directions->code true))
         (format t "~2%~a:" label2)))))

(defstage if (condition true &optional false)
  (multiple-value-bind (val constantp) (stage/constant-value condition)
    (if constantp
        (if val
            (stage-directions->code true)
            (stage-directions->code false))
        (stage-directions->test condition true false))))

(defun field->label (field)
  (cond
    ((and (consp field)
          (eql 'var (car field)))
     (format nil "ScriptVar_~a"
             (pascal-case (second field))))
    (t (error "Unknown field type ~s" field))))

(defstage set (var value)
  (stage-directions->acc value)
  (format t "~%~10tsta ~a" (field->label var)))

(defun stage-directions->code (tree)
  (unless tree (return-from stage-directions->code nil))
  (destructuring-bind (fun &rest args) tree
    (cond
      ((consp fun)
       (dolist (el tree)
         (stage-directions->code el)))
      ((symbolp fun)
       (compile-stage-direction fun args))
      (t (error "Unexpected tree function name ~s" fun)))))

(defun find-actor (actor)
  (or (when (string-equal actor 'player)
        (list :name "Player" :kind 'player :character-id #xff))
      (when (string-equal actor 'narrator)
        (list :name "Narrator" :kind 'narrator :character-id #xfe))
      (when-let (found (find-if (lambda (record)
                                  (or (string-equal actor (getf record :name))
                                      (member actor (getf record :nicks)
                                              :test #'string-equal)
                                      (string-equal actor (getf record :name))))
                                *actors*))
        found)))

(defun find-or-load-actor (actor)
  (if-let (record (find-actor actor))
    (list record t)
    (list (load-actor actor) nil)))

(defun require-actor (actor)
  (let* ((found-in-scene
           (or (find-actor actor)
               (warn "Actor ~:(~a~) was not present in scene" actor)))
         (deets (or found-in-scene (load-actor actor))))
    (unless (getf deets :character-id)
      (setf deets (load-actor actor)))
    (unless (and deets (getf deets :character-id))
      (cerror "Continue with Norville"
              "Actor ~:(~a~) does not seem to be found in the NPC Stats file."
              actor)
      (require-actor "Norville"))
    (append deets
            (list :name actor :found-in-scene-p (when found-in-scene t)))))

(defun fountain/write-scene-start (value)
  (setf *current-scene*
        (etypecase value
          (cons (concatenate 'string
                             (pascal-case (first value))
                             "/"
                             (pascal-case (second value))))
          (string (format nil "~{~a~^/~}"
                          (mapcar #'pascal-case
                                  (split-sequence #\/ value))))))
  (format t "~% Map_~a_ID load-map"
          (substitute #\_ #\/ *current-scene*))

  (setf *actors* nil))

(defun fountain/write-speech (text)
  "Write the speech data for TEXT in text and SpeakJet forms"
  (assert (< (length text) #x100) (text)
          "Text snippet exceeds maximum length $100 ($~2,'0x = ~:*~d character~:p)"
          (length text))
  (restart-case
      (format t "
  C\" ~a\"
  SpeakJet[ ~{~10t~a~^ ~20t~a~^ ~30t~a~^ ~40t~a~^ ~50t~a~^ ~60t~a~^~%~}~60t]SpeakJet"
              (prepare-dialogue text)
              (convert-for-atarivox text))
    (reload-dictionary ()
      :report "Reload the AtariVox (SpeakJet) dictionary"
      (reload-atarivox-dictionary)
      (fountain/write-speech text)))
  (format t "~% ( ~s ) do-dialogue"
          text))

(defun dialogue-hash (text)
  (let ((intro (format nil "~{~a~}"
                       (mapcar #'string-capitalize
                               (mapcar (lambda (word)
                                         (remove-if-not #'alpha-char-p word))
                                       (split-sequence #\Space text)))))
        (hash (format nil "~36r" (sxhash text))))
    (format nil "~a_~a"
            (subseq intro 0 (min (length intro) 24))
            (subseq hash 0 (min (length hash) 6)))))

(defun fountain/write-speech-branch (option text)
  "Write the speech data for TEXT in text and SpeakJet forms as the label for script destination OPTION"
  (tagbody top
     (restart-case
         (format t "
  C\" ~a\"
  add-dialogue-branch-option "
                 (prepare-dialogue text))
       (reload-dictionary ()
         :report "Reload the AtariVox (SpeakJet) dictionary"
         (reload-atarivox-dictionary)
         (go top))))
  (format t " C\" ~a\"
0 ( TODO: #1222 SpeakJet )
do-branching-dialogue ~a"
          text
          (if (string-equal "CONTINUE" option)
              "0 ( continue )"
              (concatenate 'string "ScriptLabel_"
                           (pascal-case option))))
  (return-from fountain/write-speech-branch
    (dialogue-hash text)))

(defun write-off-camera-speaker (actor-name)
  (unless (find-actor actor-name)
    (warn "Teleporting actor ~:(~a~) to the ass-end of nowhere so they can speak off-camera"
          actor-name)
    (compile-stage-direction 'enter (list actor-name :oc))
    (format t "~% ActionNonInteractive CharacterID_~a character-action!"
            (pascal-case (string actor-name))))
  (if (string-equal actor-name 'narrator)
      (format t "~% off-camera-narrator")
      (format t "~% CharacterID_~a off-camera-speaker"
              (pascal-case (string actor-name)))))

(defun compile-fountain-stream (fountain)
  "Compile the contents of the stream FOUNTAIN into assembly language"
  (if *current-pathname*
      (format t "~% ( Compiled from ~a ) "
              (enough-namestring *current-pathname*))
      (format t "~% ( Compiled from input stream )  "))
  (let ((lexer (make-fountain-lexer fountain))
        (*actors* nil)
        (*line-number* 0)
        (*fountain-state* nil))
    (loop (multiple-value-bind (sym value)
              (restart-case (funcall lexer)
                (continue ()
                  :report (lambda (s) (format s "End the script there"))
                  (format t "~3% C\" abnd\" ( \"Abnormal ending\" ) log-fault~3%")
                  (return 'abend)))
            #+() (format *trace-output* "~%~s: ~s" sym value)
            (case sym
              (stage (stage-directions->code value))
              (jump
               (format t "~2%( Script continues in another asset… )")
               (compile-stage-direction 'jump value)
               (return))
              (metadata
               (format t "~% ( ~a )" value))
              (comment
                (format t "~% ( ~a )" value))
              (label
               (format t "~% ( ~a LABEL FIXME: #1240 )"
                       (pascal-case value)))
              (scene
               (fountain/write-scene-start value))
              (speaker-oc
               (destructuring-bind (&key name &allow-other-keys) (require-actor value)
                 (write-off-camera-speaker name)))
              (speaker
               (destructuring-bind (&key name found-in-scene-p &allow-other-keys)
                   (require-actor value)
                 (cond
                   ((string-equal value 'narrator)
                    (write-off-camera-speaker name))
                   (found-in-scene-p
                    (format t "~% CharacterID_~a "
                            (pascal-case (string name))))
                   (t (cerror "Continue, with them speaking from off-camera"
                              "Actor ~:(~a~) was asked to speak, but they are not in the scene" name)
                      (write-off-camera-speaker name)))))
              (speech (fountain/write-speech value))
              (reboot
               (format t "~% reboot")
               (return))
              (game-over
               (assert (member value '("TITLE" "LOST" "WON") :test 'string-equal)
                       (value)
                       "GAME OVER currently supports TITLE, LOST, or WON only, not ~s" value)
               (format t "~% GameOverKind~:(~a~) game-over"
                       value))
              (end
               (format t "~% BYE ( ~a )"
                       (or (presence value) "End of file."))
               (return))
              (fade-to
               (format t "
FadeSpeedNormal FadingSpeed C!
FadeColor~:(~a~) FadingTarget C!"
                       (pascal-case (string value))))
              (branch
               (destructuring-bind (option . text) value
                 (fountain/write-speech-branch option text)))
              (go (format t "~%~10t ( jmp ScriptLabel_~a FIXME: #1240 )~%"
                          (pascal-case value)))
              (otherwise
               (cerror "Insert a no-op"
                       "An unhandled feature was encountered in the script. ~
 Symbol: ~s Value: ~s"
                       sym value)
               (format nil "~2%~10tnop~32t;; ~s ~s~2%"
                       sym value)))
            sym)))
  (format t "~2%( end of script file. ) ~%"))

(defun compile-fountain-string (string)
  "Compile Fountain script in STRING "
  (with-input-from-string (fountain string)
    (let ((*current-pathname* (format nil "(a ~:d character-long string)"
                                      (length string))))
      (compile-fountain-stream fountain))))

(defmacro with-forth-file-wrappers (() &body body)
  `(prog2
       (format t "~% ( -*- forth -*- )
( This file is compiled from Fountain sources. )
( Alterations to this generated file will be discarded. ) ")
       (progn ,@body)
     (format t "~2&( End of Forth sources. )~%")))

(defun compile-script (from forth)
  "Compile Fountain file with pathname FROM into Forth-alike source code file with pathname FORTH"
  (let (victoryp)
    (unwind-protect
         (tagbody top
            (let ((*actors* (list nil)))
              (restart-case
                  (progn (format *trace-output* "~2& Compiling ~a ~%~10t→ ~a …"
                                 (enough-namestring from)
                                 (enough-namestring forth))
                         (force-output *trace-output*)
                         (with-output-to-file (*standard-output* forth :if-does-not-exist :create
                                                                       :if-exists :supersede)
                           (with-forth-file-wrappers ()
                             (compile-fountain-script from)))
                         (format *trace-output* " Forth script ready to compile.")
                         (force-output *trace-output*)
                         (setf victoryp t))
                (reload-script ()
                  :report (lambda (s) (format s "Reload script ~a" (enough-namestring from)))
                  (go top))
                (reload-npc-stats () :report "Reload NPC stats from Source/Tables/NPCStats.ods"
                  (load-npc-stats)
                  (go top))))))
    (unless victoryp
      (ignore-errors (delete-file forth)))))

(defun compile-forth (forth to)
  "Compile the Forth program FORTH into the assembly sources TO"
  (let (victoryp)
    (unwind-protect
         (progn (format *trace-output* "~2% Compiling ~a~%~10t→ ~a …"
                        (enough-namestring forth)
                        (enough-namestring to))
                (force-output *trace-output*)
                (with-output-to-file (*standard-output* to :if-does-not-exist :create
                                                           :if-exists :supersede)
                  (format t ";;; This is a generated file, from ~s
~10t.enc \"minifont\"

~{~a~^_~}: .block~2%"
                          (enough-namestring forth)
                          (split-sequence #\. (pathname-name to)))
                  (with-input-from-file (*standard-input* forth)
                    (let ((*forth-file* forth))
                      (compile-forth-script)))
                  (format t "~2%~10t.bend~%"))
                (format *trace-output* " Assembly source ready to compile.")
                (force-output *trace-output*)
                (setf victoryp t))
      (unless victoryp
        (ignore-errors (delete-file to))))))

(defvar *npc-stats* nil)

(defun find-npc-stats (name)
  (unless *npc-stats* (load-npc-stats))
  (loop for npc in *npc-stats*
        when (or (string-equal (getf npc :name) name)
                 (member name (getf npc :nicks) :test #'string-equal))
          do (return npc)))

(defun load-npc-stats (&optional (pathname "Source/Tables/NPCStats.ods"))
  "Load the NPC stats table from PATHNAME"
  (format *trace-output* "~&Reading NPC stats from “~a” …"
          (enough-namestring pathname))
  (let ((lol (ss->lol (first (read-ods-into-lists pathname)))))
    (loop for i from 0 below (length lol)
          do (setf (getf (elt lol i) :character-id) i
                   (getf (elt lol i) :nicks)
                   (remove-if #'null
                              (mapcar (lambda (n)
                                        (and n (string-trim " " n)))
                                      (split-sequence #\,
                                                      (getf (elt lol i) :nicks))))))
    (setf *npc-stats* (remove-if (lambda (char) (emptyp (getf char :name))) lol))
    (format *trace-output* " … now I know about ~:d non-player character~:p"
            (length *npc-stats*))
    *npc-stats*))

(defun load-boats (&optional (pathname "Source/Tables/Boats.ods"))
  "Load the registry of boats from PATHNAME"
  (unless (and *boat-ids* *boat-classes*)
    (format *trace-output* "~&Reading boats from “~a” …" (enough-namestring pathname))
    (let ((lol (ss->lol (first (read-ods-into-lists pathname))))
          (boat-ids (make-hash-table :test 'equalp))
          (boat-classes (make-hash-table :test 'equalp)))
      (dotimes (i (length lol))
        (setf (gethash (getf (elt lol i) :boat) boat-ids)
              i
              (gethash (getf (elt lol i) :boat) boat-classes)
              (getf (elt lol i) :class)))
      (setf *boat-ids* boat-ids
            *boat-classes* boat-classes))
    (format *trace-output* " … now I know about ~:d boat~:p" (hash-table-count *boat-ids*))))

(defun find-script-id (script-moniker)
  "Find the script ID for SCRIPT-MONIKER, defined by a scene number or the hash of its name"
  (let* ((path (mapcar #'pascal-case
                       (flatten (mapcar (curry #'split-sequence #\/)
                                        (split-sequence #\- script-moniker)))))
         (dir (mapcar (lambda (el)
                        (pascal-case (string-trim #(#\Space #\Tab) el)))
                      (butlast path)))
         (dir (if (equal "Scripts" (first dir))
                  (subseq dir 1)
                  dir))
         (title (string-trim #(#\Space #\Tab) (last-elt path)))
         (pathname (make-pathname
                    :directory (append (list :relative "Source" "Scripts") dir)
                    :name title
                    :type "fountain"))
         (dir-hash (ash
                    (reduce #'logxor
                            (mapcar (lambda (ch) (- (char-code ch) (char-code #\A)))
                                    (remove-if-not #'upper-case-p
                                                   (coerce (first dir) 'list))))
                    11)))
    (unless (probe-file pathname)
      (error "Could not find expected script file “~a” for script named “~a”"
             (enough-namestring pathname) title))
    (with-input-from-file (fountain pathname)
      (loop for line = (read-line fountain nil nil)
            while line
            do (when-let (matches (cl-ppcre:all-matches "^(INT|EXT).*#[0-9]+#" line))
                 (let* ((scene-number (parse-integer
                                       (subseq line (1+ (position #\# line)))
                                       :junk-allowed t))
                        (id (logior dir-hash scene-number)))
                   (check-type scene-number (integer 0 #x7ff)
                               "a scene number integer between 0 and 2,047")
                   (format *trace-output*
                           "~&//* Script “~{~a/~}~a” is scene ~:d in locale ~:d; id $~4,'0x"
                           dir title scene-number dir-hash id)
                   (return-from find-script-id id))))
      (let ((id (logior dir-hash (logand #x7ff (sxhash title)))))
        (format *trace-output* "~&//* Script “~a” is scene ~:d in locale ~:d; id $~4,'0x"
                script-moniker (logand #x7ff id) dir-hash id)
        id))))



(defmethod output-actor-value (actor (column (eql :basic-object-class-i-d)))
  (format nil "~10t.byte ~aClass" (or (pascal-case (string (getf actor :class)))
                                      "NonPlayerCharacter")))

(defmethod output-actor-value (actor (column (eql :entity-decal)))
  (format nil "~10t.byte $ff"))

(defmethod output-actor-value (actor (column (eql :character-h-p)))
  (format nil "~10t.word ~5,'0d" (or (getf actor :hp) 2)))

(defmethod output-actor-value (actor (column (eql :character-max-h-p)))
  (format nil "~10t.word ~5,'0d" (or (getf actor :hp) 2)))

(defmethod output-actor-value (actor (column (eql :character-action)))
  (format nil "~10t.byte ActionIdle"))

(defmethod output-actor-value (actor (column (eql :actor-facing)))
  (format nil "~10t.byte ActorFacingDown"))

(defmethod output-actor-value (actor (column (eql :actor-flags)))
  (format nil "~10t.byte 0"))

(defmethod output-actor-value (actor (column (eql :actor-course)))
  (format nil "~10t.word $0000"))

(defmethod output-actor-value (actor (column (eql :character-decal-kind)))
  (format nil "~10t.byte DecalKind~a" (pascal-case (string (getf actor :decal)))))

(defmethod output-actor-value (actor (column (eql :character-skin-color)))
  (format nil "~10t.byte PaletteColor_~a"
          (pascal-case (or (getf actor :skin)
                           (progn (cerror (format nil "Continue, using default ~a" *default-skin-color*)
                                          "Character ~:(~a~) missing skin color" (getf actor :name))
                                  *default-skin-color*)))))

(defmethod output-actor-value (actor (column (eql :character-hair-color)))
  (format nil "~10t.byte PaletteColor_~a"
          (pascal-case (or (getf actor :hair)
                           (progn (cerror (format nil "Continue, using default ~a" *default-hair-color*)
                                          "Character ~:(~a~) missing hair color" (getf actor :name))
                                  *default-hair-color*)))))

(defmethod output-actor-value (actor (column (eql :character-clothes-color)))
  (format nil "~10t.byte PaletteColor_~a"
          (pascal-case (or (getf actor :clothing)
                           (progn (cerror (format nil "Continue, using default ~a" *default-clothes-color*)
                                          "Character ~:(~a~) missing clothes color" (getf actor :name))
                                  *default-clothes-color*)))))

(defmethod output-actor-value (actor (column (eql :character-head)))
  (format nil "~10t.byte ~d" (or (getf actor :head) 0)))

(defmethod output-actor-value (actor (column (eql :character-body)))
  (format nil "~10t.byte ~d" (let ((body (getf actor :body)))
                               (cond
                                 ((string-equal body "tunic") 0)
                                 ((string-equal body "robe") 1)
                                 (t (or (ignore-errors (parse-integer body)) 0))))))

(defmethod output-actor-value (actor (column (eql :character-shield)))
  (format nil "~10t.byte $~2,'0x" (or (getf actor :shield) #x80)))

(defmethod output-actor-value (actor (column (eql :character-equipment)))
  (format nil "~10t.byte $~2,'0x" (or (getf actor :equipment) #x80)))

(defmethod output-actor-value (actor (column (eql :character-armor-class)))
  (format nil "~10t.byte $~2,'0x" (or (getf actor :armor-class) 10)))

(defmethod output-actor-value (actor (column (eql :character-inventory)))
  (format nil "~10t.dword 0, 0"))

(defmethod output-actor-value (actor (column (eql :character-crowns)))
  (format nil "~10t.word ~5,'0d" (or (getf actor :crowns) 0)))

(defmethod output-actor-value (actor (column (eql :character-arrows)))
  (format nil "~10t.byte ~d" (or (getf actor :arrows) 0)))

(defmethod output-actor-value (actor (column (eql :character-potions)))
  (format nil "~10t.byte ~d" (or (getf actor :potions) 0)))

(defmethod output-actor-value (actor (column (eql :character-chalice)))
  (format nil "~10t.byte ~d" (or (getf actor :chalice) 0)))

(defmethod output-actor-value (actor (column (eql :character-gender)))
  (format nil "~10t.byte Gender~a" (pascal-case
                                    (string
                                     (let ((g (getf actor :gender #\N)))
                                       (case (char (string g) 0)
                                         (#\M :male)
                                         (#\F :female)
                                         (otherwise :nonbinary)))))))

(defmethod output-actor-value (actor (column (eql :character-character-i-d)))
  (format nil "~10t.byte $~2,'0x" (getf actor :character-id)))

(defmethod output-actor-value (actor (column (eql :character-speech-pitch)))
  (format nil "~10t.byte ~d" (or (getf actor :speech-pitch) 90)))

(defmethod output-actor-value (actor (column (eql :character-speech-bend)))
  (format nil "~10t.byte ~d" (or (getf actor :speech-bend) 5)))

(defmethod output-actor-value (actor (column (eql :character-speech-speed)))
  (format nil "~10t.byte ~d" (or (getf actor :speech-speed) 90)))

(defmethod output-actor-value (actor (column (eql :character-speech-color)))
  (format nil "~10t.byte CoLu(COL~a, $f)" (string-upcase (pascal-case (or (getf actor :speech-color) "Gray")))))

(defmethod output-actor-value (actor (column (eql :non-player-character-tactical-goal)))
  (format nil "~10t.byte 0"))

(defmethod output-actor-value (actor (column (eql :non-player-character-tactical-object)))
  (format nil "~10t.word 0"))

(defmethod output-actor-value (actor (column (eql :non-player-character-strategic-goal)))
  (format nil "~10t.byte 0"))

(defmethod output-actor-value (actor (column (eql :non-player-character-strategic-object)))
  (format nil "~10t.word 0"))

(defmethod output-actor-value (actor (column (eql :character-name-length)))
  (format nil "~10t.ptext \"~a\"" (getf actor :name)))

(defun print-one-actor-prototype (name class actor)
  (format t "~%;;;~|~2%~10tAllActors ..= [[ Character_~a, ~aClass, ~aSize ]]"
          (pascal-case (string name))
          (pascal-case (string class))
          (pascal-case (string class)))
  (format t "~%;;; ~|~%Character_~a:" (pascal-case (string name)))
  (dolist (column '(basic-object-class-i-d entity-decal character-h-p
                    character-max-h-p
                    character-action actor-facing
                    actor-course character-decal-kind
                    character-skin-color character-hair-color
                    character-clothes-color character-head character-body
                    character-shield character-equipment
                    character-armor-class character-crowns
                    character-arrows character-potions
                    character-character-i-d character-speech-pitch
                    character-speech-bend character-speech-speed
                    character-speech-color
                    non-player-character-tactical-goal
                    non-player-character-tactical-object
                    non-player-character-strategic-goal
                    non-player-character-strategic-object
                    character-name-length))
    (format t "~2%~10t* = Character_~a + ~a~%~a"
            (pascal-case (string name)) (pascal-case (string column))
            (output-actor-value actor (make-keyword (string-upcase (string column))))))
  (format t "~%~10t* = Character_~a + ~aSize"
          (pascal-case (string name)) (pascal-case (string class))))

(defun print-actor-prototypes ()
  (format t "~&;;; Generated character prototype data from NPC Stats file")
  (format t "~%~10tAllActors := []")
  (dolist (actor (load-npc-stats))
    (destructuring-bind (&key name decal hp ac pitch speed
                              hair-color skin-color clothes-color
                              speech-color bend
                              head body character-id class
                         &allow-other-keys)
        actor
      (when (> (length (string name)) 12)
        (let ((trunc (subseq (string name) 0 12)))
          (cerror (format nil "Continue with truncated name “~a”" trunc)
                  "Name ~s is too long, limit is 12 characters, ~s is ~:d character~:p"
                  name name (length (string name)))
          (setf name trunc)))
      (unless (member name '(player narrator) :test 'string-equal)
        (print-one-actor-prototype name class actor))))
  (format t "~2%
;;; ~|
ActorPointerL:
~10t.for ci := 0, ci < len(AllActors), ci += 1
~12t.byte <AllActors[ci][0]
~10t.next
ActorPointerH:
~10t.for ci := 0, ci < len(AllActors), ci += 1
~12t.byte >AllActors[ci][0]
~10t.next
ActorClassID:
~10t.for ci := 0, ci < len(AllActors), ci += 1
~12t.byte AllActors[ci][1]
~10t.next
ActorClassSize:
~10t.for ci := 0, ci < len(AllActors), ci += 1
~12t.byte AllActors[ci][2]
~10t.next

~10tActorsCount = len(AllActors)
"))

(defun write-actor-prototypes ()
  "Write the prototype data for NPCs to ActorPrototypes.s"
  (format *trace-output* "~&Writing NPC prototypes to ActorPrototypes.s…")
  (ensure-directories-exist #p"Source/Generated/")
  (with-output-to-file (*standard-output* #p"Source/Generated/ActorPrototypes.s"
                                          :if-exists :supersede)
    (print-actor-prototypes))
  (format *trace-output* " …done."))

(defun write-character-ids ()
  "Write the character IDs enumeration CharacterIDs.s and CharacterIDs.forth"
  (format *trace-output* "~&Writing CharacterIDs.s …")
  (ensure-directories-exist #p"Source/Generated/")
  (with-output-to-file (*standard-output* #p"Source/Generated/CharacterIDs.s"
                                          :if-exists :supersede)
    (format t "~&;;; Generated character ID data from NPC Stats file~2%")
    (dolist (actor (load-npc-stats))
      (destructuring-bind (&key name character-id
                           &allow-other-keys)
          actor
        (unless (member name '(player narrator) :test 'string-equal)
          (when (> (length (string name)) 12)
            (let ((trunc (subseq (string name) 0 12)))
              (cerror (format nil "Continue with truncated name “~a”" trunc)
                      "Name ~s is too long, limit is 12 characters, ~s is ~:d character~:p"
                      name name (length (string name)))
              (setf name trunc)))
          (format t "~%~10tCharacterID_~a = $~2,'0x"
                  (pascal-case (string name)) character-id))))
    (format *trace-output* " …done."))

  (format *trace-output* "~&Writing CharacterIDs.forth …")
  (ensure-directories-exist #p"Source/Generated/")
  (with-output-to-file (*standard-output* #p"Source/Generated/CharacterIDs.forth"
                                          :if-exists :supersede)
    (format t "~& ( Generated character ID data from NPC Stats file )~2%")
    (dolist (actor (load-npc-stats))
      (destructuring-bind (&key name character-id
                           &allow-other-keys)
          actor
        (unless (member name '(player narrator) :test 'string-equal)
          (format t "~%: CharacterID_~a ~d ( ~:*$~2,'0x ) ;"
                  (pascal-case (string name)) character-id))))
    (format *trace-output* " …done.")))
