fun (inv : int) ->
  (fun (color : bool) ->
     (fun (h : int) ->
        (let ((x_0 : bool)) =
           ((sizecheck : int -> bool) ((h : int) : int) : bool) in
         (match ((x_0 : bool) : bool) with
          | True ->
              ((match ((color : bool) : bool) with
                | True -> (Rbtleaf : int rbtree)
                | False ->
                    (let ((x_1 : bool)) =
                       ((frequency_gen : unit -> bool) (() : unit) : 
                       bool) in
                     ((match ((x_1 : bool) : bool) with
                       | True -> (Rbtleaf : int rbtree)
                       | False ->
                           (let ((x_2 : int rbtree)) = (Rbtleaf : int rbtree) in
                            (let ((x_3 : int)) =
                               ((int_gen : unit -> int) (() : unit) : 
                               int) in
                             (let ((x_4 : int rbtree)) =
                                (Rbtleaf : int rbtree) in
                              (Rbtnode
                                 ((true : bool),
                                   ((x_4 : int rbtree) : int rbtree),
                                   ((x_3 : int) : int),
                                   ((x_2 : int rbtree) : int rbtree)) : 
                                int rbtree) : int rbtree) : int rbtree) : 
                           int rbtree)) : int rbtree) : int rbtree)) : 
              int rbtree)
          | False ->
              ((match ((color : bool) : bool) with
                | True ->
                    (let ((x_5 : int)) =
                       ((subs : int -> int) ((inv : int) : int) : int) in
                     (let ((x_6 : bool -> int -> int rbtree)) =
                        ((rbtree_gen : int -> bool -> int -> int rbtree)
                           ((x_5 : int) : int) : bool -> int -> int rbtree) in
                      (let ((x_7 : int -> int rbtree)) =
                         ((x_6 : bool -> int -> int rbtree) (false : bool) : 
                         int -> int rbtree) in
                       (let ((x_8 : int)) =
                          ((subs : int -> int) ((h : int) : int) : int) in
                        (let ((lt2 : int rbtree)) =
                           ((x_7 : int -> int rbtree) ((x_8 : int) : int) : 
                           int rbtree) in
                         (let ((x_9 : int)) =
                            ((subs : int -> int) ((inv : int) : int) : 
                            int) in
                          (let ((x_10 : bool -> int -> int rbtree)) =
                             ((rbtree_gen : int -> bool -> int -> int rbtree)
                                ((x_9 : int) : int) : bool ->
                                                        int -> int rbtree) in
                           (let ((x_11 : int -> int rbtree)) =
                              ((x_10 : bool -> int -> int rbtree)
                                 (false : bool) : int -> int rbtree) in
                            (let ((x_12 : int)) =
                               ((subs : int -> int) ((h : int) : int) : 
                               int) in
                             (let ((rt2 : int rbtree)) =
                                ((x_11 : int -> int rbtree)
                                   ((x_12 : int) : int) : int rbtree) in
                              (let ((x_13 : int)) =
                                 ((int_gen : unit -> int) (() : unit) : 
                                 int) in
                               (Rbtnode
                                  ((false : bool),
                                    ((lt2 : int rbtree) : int rbtree),
                                    ((x_13 : int) : int),
                                    ((rt2 : int rbtree) : int rbtree)) : 
                                 int rbtree) : int rbtree) : int rbtree) : 
                              int rbtree) : int rbtree) : int rbtree) : 
                           int rbtree) : int rbtree) : int rbtree) : 
                        int rbtree) : int rbtree) : int rbtree)
                | False ->
                    (let ((c : bool)) =
                       ((frequency_gen : unit -> bool) (() : unit) : 
                       bool) in
                     ((match ((c : bool) : bool) with
                       | True ->
                           (let ((x_14 : int)) =
                              ((subs : int -> int) ((inv : int) : int) : 
                              int) in
                            (let ((x_15 : bool -> int -> int rbtree)) =
                               ((rbtree_gen : int ->
                                                bool -> int -> int rbtree)
                                  ((x_14 : int) : int) : bool ->
                                                           int -> int rbtree) in
                             (let ((x_16 : int -> int rbtree)) =
                                ((x_15 : bool -> int -> int rbtree)
                                   (true : bool) : int -> int rbtree) in
                              (let ((lt3 : int rbtree)) =
                                 ((x_16 : int -> int rbtree)
                                    ((h : int) : int) : int rbtree) in
                               (let ((x_17 : int)) =
                                  ((subs : int -> int) ((inv : int) : int) : 
                                  int) in
                                (let ((x_18 : bool -> int -> int rbtree)) =
                                   ((rbtree_gen : int ->
                                                    bool -> int -> int rbtree)
                                      ((x_17 : int) : int) : bool ->
                                                               int ->
                                                                 int rbtree) in
                                 (let ((x_19 : int -> int rbtree)) =
                                    ((x_18 : bool -> int -> int rbtree)
                                       (true : bool) : int -> int rbtree) in
                                  (let ((rt3 : int rbtree)) =
                                     ((x_19 : int -> int rbtree)
                                        ((h : int) : int) : int rbtree) in
                                   (let ((x_20 : int)) =
                                      ((int_gen : unit -> int) (() : unit) : 
                                      int) in
                                    (Rbtnode
                                       ((true : bool),
                                         ((lt3 : int rbtree) : int rbtree),
                                         ((x_20 : int) : int),
                                         ((rt3 : int rbtree) : int rbtree)) : 
                                      int rbtree) : int rbtree) : int rbtree) : 
                                   int rbtree) : int rbtree) : int rbtree) : 
                                int rbtree) : int rbtree) : int rbtree) : 
                           int rbtree)
                       | False ->
                           (let ((x_21 : int)) =
                              ((subs : int -> int) ((inv : int) : int) : 
                              int) in
                            (let ((x_22 : int)) =
                               ((subs : int -> int) ((x_21 : int) : int) : 
                               int) in
                             (let ((x_23 : bool -> int -> int rbtree)) =
                                ((rbtree_gen : int ->
                                                 bool -> int -> int rbtree)
                                   ((x_22 : int) : int) : bool ->
                                                            int -> int rbtree) in
                              (let ((x_24 : int -> int rbtree)) =
                                 ((x_23 : bool -> int -> int rbtree)
                                    (false : bool) : int -> int rbtree) in
                               (let ((x_25 : int)) =
                                  ((subs : int -> int) ((h : int) : int) : 
                                  int) in
                                (let ((lt4 : int rbtree)) =
                                   ((x_24 : int -> int rbtree)
                                      ((x_25 : int) : int) : int rbtree) in
                                 (let ((x_26 : int)) =
                                    ((subs : int -> int) ((inv : int) : int) : 
                                    int) in
                                  (let ((x_27 : int)) =
                                     ((subs : int -> int)
                                        ((x_26 : int) : int) : int) in
                                   (let ((x_28 : bool -> int -> int rbtree))
                                      =
                                      ((rbtree_gen : int ->
                                                       bool ->
                                                         int -> int rbtree)
                                         ((x_27 : int) : int) : bool ->
                                                                  int ->
                                                                    int
                                                                    rbtree) in
                                    (let ((x_29 : int -> int rbtree)) =
                                       ((x_28 : bool -> int -> int rbtree)
                                          (false : bool) : int -> int rbtree) in
                                     (let ((x_30 : int)) =
                                        ((subs : int -> int)
                                           ((h : int) : int) : int) in
                                      (let ((rt4 : int rbtree)) =
                                         ((x_29 : int -> int rbtree)
                                            ((x_30 : int) : int) : int rbtree) in
                                       (let ((x_31 : int)) =
                                          ((int_gen : unit -> int)
                                             (() : unit) : int) in
                                        (Rbtnode
                                           ((false : bool),
                                             ((lt4 : int rbtree) : int rbtree),
                                             ((x_31 : int) : int),
                                             ((rt4 : int rbtree) : int rbtree)) : 
                                          int rbtree) : int rbtree) : 
                                        int rbtree) : int rbtree) : int
                                                                    rbtree) : 
                                     int rbtree) : int rbtree) : int rbtree) : 
                                  int rbtree) : int rbtree) : int rbtree) : 
                               int rbtree) : int rbtree) : int rbtree)) : 
                       int rbtree) : int rbtree)) : int rbtree) : int rbtree) : 
        int rbtree) : int -> int rbtree) : bool -> int -> int rbtree)