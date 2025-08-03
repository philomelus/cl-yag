(in-package :cl-yag)

;;;;000001111111111222222222233333333334444444444555555555566666666667777777777

                 ;;;;0000000001111111111222222222233333333334444444444555555555566666666667777777777

(docs:define-docs (manager
                    "Given a list of objects to manager, handles interaction of user interface
events between them and the current display.

connect Events:
  char obj mgr code modfifiers
  key-down obg mgr code modifiers
  key-up obj mgr code modifiers
  mouse-click obj mgr x y b
  mouse-down obj mgr x y b
  mouse-move obj mgr x y
  mouse-moved obj mgr
  mouse-up obj mgr x y b

The following events also contain pre and post events as well:
(for example \"mouse-down-pre\", \"mouse-down-post\")
  mouse-down, mouse-up, key-down, key-up, mouse-move, paint
"))




