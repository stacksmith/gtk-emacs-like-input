(in-package #:gtk-emacs-like-input)

;;; This is a bit of a mess.  Keys come in as gtkkey codes representing
;;; keyboard codes.  These are mapped across keymaps; keymaps need an
;;; ascii representation.  I am not sure about unicode support across
;;; all Lisp implementations (CCL does not seem to (code-char #x2399) as
;;; PRINT_SCREEN_SYMBOL, for instance...
;;;   Since GTK should handle all keystrokes correctly across platforms,
;;; we shall rely on GTK codes, and translate them to Lisp chars only
;;; as necessary.

;;;   Note that #'equalp is used, making name lookup case-insensitive.
(defvar *gtkcode-name-map* (make-hash-table))
(defvar *name-gtkcode-map* (make-hash-table :test 'equalp))

(defun define-gtkcode (gtkcode name)
  "Define a mapping from a gtkcode name to a gtkcode."
  (setf (gethash gtkcode *gtkcode-name-map*) name
        (gethash name *name-gtkcode-map*) gtkcode))

(defun gtkcode-name->gtkcode (name)
  "Return the gtkcode corresponding to NAME."
  (multiple-value-bind (value present-p)
      (gethash name *name-gtkcode-map*)
    (declare (ignore present-p))
    value))

(defun gtkcode->gtkcode-name (gtkcode)
  "Return the name corresponding to GTKCODE or nil"
  (gethash gtkcode *gtkcode-name-map*))

;;OK, I will rip out X11/GTK gtkcodes and name a few the Emacs way...
(define-gtkcode #xff0d "Return")  ;Return, enter
(define-gtkcode #xff1b "ESC")
(define-gtkcode #xff09 "TAB")
(define-gtkcode #xff08 "BS")      ;Back space, back char
(define-gtkcode #xffff "DEL")     ;Delete, rubout
(define-gtkcode #x0020 "SPC")     ;U+0020 SPACE

(define-gtkcode #x0021 "!")       ;U+0021 EXCLAMATION MAR
(define-gtkcode #x0022 "\"")      ;U+0022 QUOTATION MARK
(define-gtkcode #x0023 "#")       ;U+0023 NUMBER SIGN
(define-gtkcode #x0024 "$")       ;U+0024 DOLLAR SIGN
(define-gtkcode #x0025 "%")       ;U+0025 PERCENT SIGN
(define-gtkcode #x0026 "&")       ;U+0026 AMPERSAND
(define-gtkcode #x0027 "'")       ;U+0027 APOSTROPHE
(define-gtkcode #x0028 "(")      ;U+0028 LEFT PARENTHESIS
(define-gtkcode #x0029 ")")     ;U+0029 RIGHT PARENTHESIS
(define-gtkcode #x002a "*")       ;U+002A ASTERISK
(define-gtkcode #x002b "+")           ;U+002B PLUS SIGN
(define-gtkcode #x002c ",")          ;U+002C COMMA
(define-gtkcode #x002d "-")          ;U+002D HYPHEN-MINUS
(define-gtkcode #x002e ".")         ;U+002E FULL STOP
(define-gtkcode #x002f "/")          ;U+002F SOLIDUS
(define-gtkcode #x003a ":")          ;U+003A COLON
(define-gtkcode #x003b ";")      ;U+003B SEMICOLON
(define-gtkcode #x003c "<")           ;U+003C LESS-THAN SIGN
(define-gtkcode #x003d "=")          ;U+003D EQUALS SIGN
(define-gtkcode #x003e ">")        ;U+003E GREATER-THAN SIGN
(define-gtkcode #x003f "?")       ;U+003F QUESTION MARK
(define-gtkcode #x0040 "@")             ;U+0040 COMMERCIAL AT
(define-gtkcode #x005b "[")    ;U+005B LEFT SQUARE BRACKET
(define-gtkcode #x005c "\\")      ;U+005C REVERSE SOLIDUS
(define-gtkcode #x005d "]")   ;U+005D RIGHT SQUARE BRACKET
(define-gtkcode #x005e "^")    ;U+005E CIRCUMFLEX ACCENT
(define-gtkcode #x005f "_")     ;U+005F LOW LINE
(define-gtkcode #x0060 "`")          ;U+0060 GRAVE ACCENT
(define-gtkcode #x007b "{")      ;U+007B LEFT CURLY BRACKET
(define-gtkcode #x007c "|")            ;U+007C VERTICAL LINE
(define-gtkcode #x007d "}")     ;U+007D RIGHT CURLY BRACKET
(define-gtkcode #x007e "~")     ;U+007E TILDE



(define-gtkcode #xffffff "VoidSymbol")   ;Void symbol


(define-gtkcode #xff0a "Linefeed")       ;Linefeed, LF
(define-gtkcode #xff0b "Clear")
(define-gtkcode #xff13 "Pause")          ;Pause, hold
(define-gtkcode #xff14 "Scroll_Lock")
(define-gtkcode #xff15 "Sys_Req")


(define-gtkcode #xff20 "Multi_key")      ;Multi-key character compose
(define-gtkcode #xff37 "Codeinput")
(define-gtkcode #xff3c "SingleCandidate")
(define-gtkcode #xff3d "MultipleCandidate")
(define-gtkcode #xff3e "PreviousCandidate")
(define-gtkcode #xff21 "Kanji")          ;Kanji, Kanji convert
(define-gtkcode #xff22 "Muhenkan")       ;Cancel Conversion
(define-gtkcode #xff23 "Henkan_Mode")    ;Start/Stop Conversion
(define-gtkcode #xff23 "Henkan")         ;Alias for Henkan_Mode
(define-gtkcode #xff24 "Romaji")         ;to Romaji
(define-gtkcode #xff25 "Hiragana")       ;to Hiragana
(define-gtkcode #xff26 "Katakana")       ;to Katakana
(define-gtkcode #xff27 "Hiragana_Katakana") ;Hiragana/Katakana toggle
(define-gtkcode #xff28 "Zenkaku")        ;to Zenkaku
(define-gtkcode #xff29 "Hankaku")        ;to Hankaku
(define-gtkcode #xff2a "Zenkaku_Hankaku") ;Zenkaku/Hankaku toggle
(define-gtkcode #xff2b "Touroku")        ;Add to Dictionary
(define-gtkcode #xff2c "Massyo")         ;Delete from Dictionary
(define-gtkcode #xff2d "Kana_Lock")      ;Kana Lock
(define-gtkcode #xff2e "Kana_Shift")     ;Kana Shift
(define-gtkcode #xff2f "Eisu_Shift")     ;Alphanumeric Shift
(define-gtkcode #xff30 "Eisu_toggle")    ;Alphanumeric toggle
(define-gtkcode #xff37 "Kanji_Bangou")   ;Codeinput
(define-gtkcode #xff3d "Zen_Koho")       ;Multiple/All Candidate(s)
(define-gtkcode #xff3e "Mae_Koho")       ;Previous Candidate
(define-gtkcode #xff50 "Home")
(define-gtkcode #xff51 "Left")           ;Move left, left arrow
(define-gtkcode #xff52 "Up")             ;Move up, up arrow
(define-gtkcode #xff53 "Right")          ;Move right, right arrow
(define-gtkcode #xff54 "Down")           ;Move down, down arrow
(define-gtkcode #xff55 "Prior")          ;Prior, previous
(define-gtkcode #xff55 "Page_Up")
(define-gtkcode #xff56 "Next")           ;Next
(define-gtkcode #xff56 "Page_Down")
(define-gtkcode #xff57 "End")            ;EOL
(define-gtkcode #xff58 "Begin")          ;BOL
(define-gtkcode #xff60 "Select")         ;Select, mark
(define-gtkcode #xff61 "Print")
(define-gtkcode #xff62 "Execute")        ;Execute, run, do
(define-gtkcode #xff63 "Insert")         ;Insert, insert here
(define-gtkcode #xff65 "Undo")
(define-gtkcode #xff66 "Redo")           ;Redo, again
(define-gtkcode #xff67 "Menu")
(define-gtkcode #xff68 "Find")           ;Find, search
(define-gtkcode #xff69 "Cancel")         ;Cancel, stop, abort, exit
(define-gtkcode #xff6a "Help")           ;Help
(define-gtkcode #xff6b "Break")
(define-gtkcode #xff7e "Mode_switch")    ;Character set switch
(define-gtkcode #xff7e "script_switch")  ;Alias for mode_switch
(define-gtkcode #xff7f "Num_Lock")
(define-gtkcode #xff80 "KP_Space")       ;Space
(define-gtkcode #xff89 "KP_Tab")
(define-gtkcode #xff8d "KP_Enter")       ;Enter
(define-gtkcode #xff91 "KP_F1")          ;PF1, KP_A, ...
(define-gtkcode #xff92 "KP_F2")
(define-gtkcode #xff93 "KP_F3")
(define-gtkcode #xff94 "KP_F4")
(define-gtkcode #xff95 "KP_Home")
(define-gtkcode #xff96 "KP_Left")
(define-gtkcode #xff97 "KP_Up")
(define-gtkcode #xff98 "KP_Right")
(define-gtkcode #xff99 "KP_Down")
(define-gtkcode #xff9a "KP_Prior")
(define-gtkcode #xff9a "KP_Page_Up")
(define-gtkcode #xff9b "KP_Next")
(define-gtkcode #xff9b "KP_Page_Down")
(define-gtkcode #xff9c "KP_End")
(define-gtkcode #xff9d "KP_Begin")
(define-gtkcode #xff9e "KP_Insert")
(define-gtkcode #xff9f "KP_Delete")
(define-gtkcode #xffbd "KP_Equal")       ;Equals
(define-gtkcode #xffaa "KP_Multiply")
(define-gtkcode #xffab "KP_Add")
(define-gtkcode #xffac "KP_Separator")   ;Separator, often comma
(define-gtkcode #xffad "KP_Subtract")
(define-gtkcode #xffae "KP_Decimal")
(define-gtkcode #xffaf "KP_Divide")
(define-gtkcode #xffb0 "KP_0")
(define-gtkcode #xffb1 "KP_1")
(define-gtkcode #xffb2 "KP_2")
(define-gtkcode #xffb3 "KP_3")
(define-gtkcode #xffb4 "KP_4")
(define-gtkcode #xffb5 "KP_5")
(define-gtkcode #xffb6 "KP_6")
(define-gtkcode #xffb7 "KP_7")
(define-gtkcode #xffb8 "KP_8")
(define-gtkcode #xffb9 "KP_9")
(define-gtkcode #xffbe "F1")
(define-gtkcode #xffbf "F2")
(define-gtkcode #xffc0 "F3")
(define-gtkcode #xffc1 "F4")
(define-gtkcode #xffc2 "F5")
(define-gtkcode #xffc3 "F6")
(define-gtkcode #xffc4 "F7")
(define-gtkcode #xffc5 "F8")
(define-gtkcode #xffc6 "F9")
(define-gtkcode #xffc7 "F10")
(define-gtkcode #xffc8 "F11")
(define-gtkcode #xffc9 "F12")
(define-gtkcode #xffca "F13")
(define-gtkcode #xffcb "F14")
(define-gtkcode #xffcc "F15")
(define-gtkcode #xffcd "F16")
(define-gtkcode #xffce "F17")
(define-gtkcode #xffcf "F18")
(define-gtkcode #xffd0 "F19")
(define-gtkcode #xffd1 "F20")
(define-gtkcode #xffd2 "F21")
(define-gtkcode #xffd3 "F22")
(define-gtkcode #xffd4 "F23")
(define-gtkcode #xffd5 "F24")
(define-gtkcode #xffd6 "F25")
(define-gtkcode #xffd7 "F26")
(define-gtkcode #xffd8 "F27")
(define-gtkcode #xffd9 "F28")
(define-gtkcode #xffda "F29")
(define-gtkcode #xffdb "F30")
(define-gtkcode #xffdc "F31")
(define-gtkcode #xffdd "F32")
(define-gtkcode #xffde "F33")
(define-gtkcode #xffdf "F34")
(define-gtkcode #xffe0 "F35")
(define-gtkcode #xffe1 "Shift_L")        ;Left shift
(define-gtkcode #xffe2 "Shift_R")        ;Right shift
(define-gtkcode #xffe3 "Control_L")      ;Left control
(define-gtkcode #xffe4 "Control_R")      ;Right control
(define-gtkcode #xffe5 "Caps_Lock")      ;Caps lock
(define-gtkcode #xffe6 "Shift_Lock")     ;Shift lock
(define-gtkcode #xffe7 "Meta_L")         ;Left meta
(define-gtkcode #xffe8 "Meta_R")         ;Right meta
(define-gtkcode #xffe9 "Alt_L")          ;Left alt
(define-gtkcode #xffea "Alt_R")          ;Right alt
(define-gtkcode #xffeb "Super_L")        ;Left super
(define-gtkcode #xffec "Super_R")        ;Right super
(define-gtkcode #xffed "Hyper_L")        ;Left hyper
(define-gtkcode #xffee "Hyper_R")        ;Right hyper
(define-gtkcode #xfe01 "ISO_Lock")
(define-gtkcode #xfe02 "ISO_Level2_Latch")
(define-gtkcode #xfe03 "ISO_Level3_Shift")
(define-gtkcode #xfe04 "ISO_Level3_Latch")
(define-gtkcode #xfe05 "ISO_Level3_Lock")
(define-gtkcode #xff7e "ISO_Group_Shift") ;Alias for mode_switch
(define-gtkcode #xfe06 "ISO_Group_Latch")
(define-gtkcode #xfe07 "ISO_Group_Lock")
(define-gtkcode #xfe08 "ISO_Next_Group")
(define-gtkcode #xfe09 "ISO_Next_Group_Lock")
(define-gtkcode #xfe0a "ISO_Prev_Group")
(define-gtkcode #xfe0b "ISO_Prev_Group_Lock")
(define-gtkcode #xfe0c "ISO_First_Group")
(define-gtkcode #xfe0d "ISO_First_Group_Lock")
(define-gtkcode #xfe0e "ISO_Last_Group")
(define-gtkcode #xfe0f "ISO_Last_Group_Lock")
(define-gtkcode #xfe20 "ISO_Left_Tab")
(define-gtkcode #xfe21 "ISO_Move_Line_Up")
(define-gtkcode #xfe22 "ISO_Move_Line_Down")
(define-gtkcode #xfe23 "ISO_Partial_Line_Up")
(define-gtkcode #xfe24 "ISO_Partial_Line_Down")
(define-gtkcode #xfe25 "ISO_Partial_Space_Left")
(define-gtkcode #xfe26 "ISO_Partial_Space_Right")
(define-gtkcode #xfe27 "ISO_Set_Margin_Left")
(define-gtkcode #xfe28 "ISO_Set_Margin_Right")
(define-gtkcode #xfe29 "ISO_Release_Margin_Left")
(define-gtkcode #xfe2a "ISO_Release_Margin_Right")
(define-gtkcode #xfe2b "ISO_Release_Both_Margins")
(define-gtkcode #xfe2c "ISO_Fast_Cursor_Left")
(define-gtkcode #xfe2d "ISO_Fast_Cursor_Right")
(define-gtkcode #xfe2e "ISO_Fast_Cursor_Up")
(define-gtkcode #xfe2f "ISO_Fast_Cursor_Down")
(define-gtkcode #xfe30 "ISO_Continuous_Underline")
(define-gtkcode #xfe31 "ISO_Discontinuous_Underline")
(define-gtkcode #xfe32 "ISO_Emphasize")
(define-gtkcode #xfe33 "ISO_Center_Object")
(define-gtkcode #xfe34 "ISO_Enter")
(define-gtkcode #xfe50 "dead_grave")
(define-gtkcode #xfe51 "dead_acute")
(define-gtkcode #xfe52 "dead_circumflex")
(define-gtkcode #xfe53 "dead_tilde")
(define-gtkcode #xfe54 "dead_macron")
(define-gtkcode #xfe55 "dead_breve")
(define-gtkcode #xfe56 "dead_abovedot")
(define-gtkcode #xfe57 "dead_diaeresis")
(define-gtkcode #xfe58 "dead_abovering")
(define-gtkcode #xfe59 "dead_doubleacute")
(define-gtkcode #xfe5a "dead_caron")
(define-gtkcode #xfe5b "dead_cedilla")
(define-gtkcode #xfe5c "dead_ogonek")
(define-gtkcode #xfe5d "dead_iota")
(define-gtkcode #xfe5e "dead_voiced_sound")
(define-gtkcode #xfe5f "dead_semivoiced_sound")
(define-gtkcode #xfe60 "dead_belowdot")
(define-gtkcode #xfe61 "dead_hook")
(define-gtkcode #xfe62 "dead_horn")
(define-gtkcode #xfed0 "First_Virtual_Screen")
(define-gtkcode #xfed1 "Prev_Virtual_Screen")
(define-gtkcode #xfed2 "Next_Virtual_Screen")
(define-gtkcode #xfed4 "Last_Virtual_Screen")
(define-gtkcode #xfed5 "Terminate_Server")
(define-gtkcode #xfe70 "AccessX_Enable")
(define-gtkcode #xfe71 "AccessX_Feedback_Enable")
(define-gtkcode #xfe72 "RepeatKeys_Enable")
(define-gtkcode #xfe73 "SlowKeys_Enable")
(define-gtkcode #xfe74 "BounceKeys_Enable")
(define-gtkcode #xfe75 "StickyKeys_Enable")
(define-gtkcode #xfe76 "MouseKeys_Enable")
(define-gtkcode #xfe77 "MouseKeys_Accel_Enable")
(define-gtkcode #xfe78 "Overlay1_Enable")
(define-gtkcode #xfe79 "Overlay2_Enable")
(define-gtkcode #xfe7a "AudibleBell_Enable")
(define-gtkcode #xfee0 "Pointer_Left")
(define-gtkcode #xfee1 "Pointer_Right")
(define-gtkcode #xfee2 "Pointer_Up")
(define-gtkcode #xfee3 "Pointer_Down")
(define-gtkcode #xfee4 "Pointer_UpLeft")
(define-gtkcode #xfee5 "Pointer_UpRight")
(define-gtkcode #xfee6 "Pointer_DownLeft")
(define-gtkcode #xfee7 "Pointer_DownRight")
(define-gtkcode #xfee8 "Pointer_Button_Dflt")
(define-gtkcode #xfee9 "Pointer_Button1")
(define-gtkcode #xfeea "Pointer_Button2")
(define-gtkcode #xfeeb "Pointer_Button3")
(define-gtkcode #xfeec "Pointer_Button4")
(define-gtkcode #xfeed "Pointer_Button5")
(define-gtkcode #xfeee "Pointer_DblClick_Dflt")
(define-gtkcode #xfeef "Pointer_DblClick1")
(define-gtkcode #xfef0 "Pointer_DblClick2")
(define-gtkcode #xfef1 "Pointer_DblClick3")
(define-gtkcode #xfef2 "Pointer_DblClick4")
(define-gtkcode #xfef3 "Pointer_DblClick5")
(define-gtkcode #xfef4 "Pointer_Drag_Dflt")
(define-gtkcode #xfef5 "Pointer_Drag1")
(define-gtkcode #xfef6 "Pointer_Drag2")
(define-gtkcode #xfef7 "Pointer_Drag3")
(define-gtkcode #xfef8 "Pointer_Drag4")
(define-gtkcode #xfefd "Pointer_Drag5")
(define-gtkcode #xfef9 "Pointer_EnableKeys")
(define-gtkcode #xfefa "Pointer_Accelerate")
(define-gtkcode #xfefb "Pointer_DfltBtnNext")
(define-gtkcode #xfefc "Pointer_DfltBtnPrev")
(define-gtkcode #xfd01 "3270_Duplicate")
(define-gtkcode #xfd02 "3270_FieldMark")
(define-gtkcode #xfd03 "3270_Right2")
(define-gtkcode #xfd04 "3270_Left2")
(define-gtkcode #xfd05 "3270_BackTab")
(define-gtkcode #xfd06 "3270_EraseEOF")
(define-gtkcode #xfd07 "3270_EraseInput")
(define-gtkcode #xfd08 "3270_Reset")
(define-gtkcode #xfd09 "3270_Quit")
(define-gtkcode #xfd0a "3270_PA1")
(define-gtkcode #xfd0b "3270_PA2")
(define-gtkcode #xfd0c "3270_PA3")
(define-gtkcode #xfd0d "3270_Test")
(define-gtkcode #xfd0e "3270_Attn")
(define-gtkcode #xfd0f "3270_CursorBlink")
(define-gtkcode #xfd10 "3270_AltCursor")
(define-gtkcode #xfd11 "3270_KeyClick")
(define-gtkcode #xfd12 "3270_Jump")
(define-gtkcode #xfd13 "3270_Ident")
(define-gtkcode #xfd14 "3270_Rule")
(define-gtkcode #xfd15 "3270_Copy")
(define-gtkcode #xfd16 "3270_Play")
(define-gtkcode #xfd17 "3270_Setup")
(define-gtkcode #xfd18 "3270_Record")
(define-gtkcode #xfd19 "3270_ChangeScreen")
(define-gtkcode #xfd1a "3270_DeleteWord")
(define-gtkcode #xfd1b "3270_ExSelect")
(define-gtkcode #xfd1c "3270_CursorSelect")
(define-gtkcode #xfd1d "3270_PrintScreen")
(define-gtkcode #xfd1e "3270_Enter")



(define-gtkcode #x0030 "0")              ;U+0030 DIGIT ZERO
(define-gtkcode #x0031 "1")              ;U+0031 DIGIT ONE
(define-gtkcode #x0032 "2")              ;U+0032 DIGIT TWO
(define-gtkcode #x0033 "3")              ;U+0033 DIGIT THREE
(define-gtkcode #x0034 "4")              ;U+0034 DIGIT FOUR
(define-gtkcode #x0035 "5")              ;U+0035 DIGIT FIVE
(define-gtkcode #x0036 "6")              ;U+0036 DIGIT SIX
(define-gtkcode #x0037 "7")              ;U+0037 DIGIT SEVEN
(define-gtkcode #x0038 "8")              ;U+0038 DIGIT EIGHT
(define-gtkcode #x0039 "9")              ;U+0039 DIGIT NINE

(define-gtkcode #x0041 "A")              ;U+0041 LATIN CAPITAL LETTER A
(define-gtkcode #x0042 "B")              ;U+0042 LATIN CAPITAL LETTER B
(define-gtkcode #x0043 "C")              ;U+0043 LATIN CAPITAL LETTER C
(define-gtkcode #x0044 "D")              ;U+0044 LATIN CAPITAL LETTER D
(define-gtkcode #x0045 "E")              ;U+0045 LATIN CAPITAL LETTER E
(define-gtkcode #x0046 "F")              ;U+0046 LATIN CAPITAL LETTER F
(define-gtkcode #x0047 "G")              ;U+0047 LATIN CAPITAL LETTER G
(define-gtkcode #x0048 "H")              ;U+0048 LATIN CAPITAL LETTER H
(define-gtkcode #x0049 "I")              ;U+0049 LATIN CAPITAL LETTER I
(define-gtkcode #x004a "J")              ;U+004A LATIN CAPITAL LETTER J
(define-gtkcode #x004b "K")              ;U+004B LATIN CAPITAL LETTER K
(define-gtkcode #x004c "L")              ;U+004C LATIN CAPITAL LETTER L
(define-gtkcode #x004d "M")              ;U+004D LATIN CAPITAL LETTER M
(define-gtkcode #x004e "N")              ;U+004E LATIN CAPITAL LETTER N
(define-gtkcode #x004f "O")              ;U+004F LATIN CAPITAL LETTER O
(define-gtkcode #x0050 "P")              ;U+0050 LATIN CAPITAL LETTER P
(define-gtkcode #x0051 "Q")              ;U+0051 LATIN CAPITAL LETTER Q
(define-gtkcode #x0052 "R")              ;U+0052 LATIN CAPITAL LETTER R
(define-gtkcode #x0053 "S")              ;U+0053 LATIN CAPITAL LETTER S
(define-gtkcode #x0054 "T")              ;U+0054 LATIN CAPITAL LETTER T
(define-gtkcode #x0055 "U")              ;U+0055 LATIN CAPITAL LETTER U
(define-gtkcode #x0056 "V")              ;U+0056 LATIN CAPITAL LETTER V
(define-gtkcode #x0057 "W")              ;U+0057 LATIN CAPITAL LETTER W
(define-gtkcode #x0058 "X")              ;U+0058 LATIN CAPITAL LETTER X
(define-gtkcode #x0059 "Y")              ;U+0059 LATIN CAPITAL LETTER Y
(define-gtkcode #x005a "Z")              ;U+005A LATIN CAPITAL LETTER Z
(define-gtkcode #x0061 "a")              ;U+0061 LATIN SMALL LETTER A
(define-gtkcode #x0062 "b")              ;U+0062 LATIN SMALL LETTER B
(define-gtkcode #x0063 "c")              ;U+0063 LATIN SMALL LETTER C
(define-gtkcode #x0064 "d")              ;U+0064 LATIN SMALL LETTER D
(define-gtkcode #x0065 "e")              ;U+0065 LATIN SMALL LETTER E
(define-gtkcode #x0066 "f")              ;U+0066 LATIN SMALL LETTER F
(define-gtkcode #x0067 "g")              ;U+0067 LATIN SMALL LETTER G
(define-gtkcode #x0068 "h")              ;U+0068 LATIN SMALL LETTER H
(define-gtkcode #x0069 "i")              ;U+0069 LATIN SMALL LETTER I
(define-gtkcode #x006a "j")              ;U+006A LATIN SMALL LETTER J
(define-gtkcode #x006b "k")              ;U+006B LATIN SMALL LETTER K
(define-gtkcode #x006c "l")              ;U+006C LATIN SMALL LETTER L
(define-gtkcode #x006d "m")              ;U+006D LATIN SMALL LETTER M
(define-gtkcode #x006e "n")              ;U+006E LATIN SMALL LETTER N
(define-gtkcode #x006f "o")              ;U+006F LATIN SMALL LETTER O
(define-gtkcode #x0070 "p")              ;U+0070 LATIN SMALL LETTER P
(define-gtkcode #x0071 "q")              ;U+0071 LATIN SMALL LETTER Q
(define-gtkcode #x0072 "r")              ;U+0072 LATIN SMALL LETTER R
(define-gtkcode #x0073 "s")              ;U+0073 LATIN SMALL LETTER S
(define-gtkcode #x0074 "t")              ;U+0074 LATIN SMALL LETTER T
(define-gtkcode #x0075 "u")              ;U+0075 LATIN SMALL LETTER U
(define-gtkcode #x0076 "v")              ;U+0076 LATIN SMALL LETTER V
(define-gtkcode #x0077 "w")              ;U+0077 LATIN SMALL LETTER W
(define-gtkcode #x0078 "x")              ;U+0078 LATIN SMALL LETTER X
(define-gtkcode #x0079 "y")              ;U+0079 LATIN SMALL LETTER Y
(define-gtkcode #x007a "z")              ;U+007A LATIN SMALL LETTER Z
(define-gtkcode #x00a0 "nobreakspace")   ;U+00A0 NO-BREAK SPACE
(define-gtkcode #x00a1 "exclamdown")  ;U+00A1 INVERTED EXCLAMATION MARK
(define-gtkcode #x00a2 "cent")           ;U+00A2 CENT SIGN
(define-gtkcode #x00a3 "sterling")       ;U+00A3 POUND SIGN
(define-gtkcode #x00a4 "currency")       ;U+00A4 CURRENCY SIGN
(define-gtkcode #x00a5 "yen")            ;U+00A5 YEN SIGN
(define-gtkcode #x00a6 "brokenbar")      ;U+00A6 BROKEN BAR
(define-gtkcode #x00a7 "section")        ;U+00A7 SECTION SIGN
(define-gtkcode #x00a8 "diaeresis")      ;U+00A8 DIAERESIS
(define-gtkcode #x00a9 "copyright")      ;U+00A9 COPYRIGHT SIGN
(define-gtkcode #x00aa "ordfeminine") ;U+00AA FEMININE ORDINAL INDICATOR
(define-gtkcode #x00ab "guillemotleft") ;U+00AB LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
(define-gtkcode #x00ac "notsign")        ;U+00AC NOT SIGN
(define-gtkcode #x00ad "hyphen")         ;U+00AD SOFT HYPHEN
(define-gtkcode #x00ae "registered")     ;U+00AE REGISTERED SIGN
(define-gtkcode #x00af "macron")         ;U+00AF MACRON
(define-gtkcode #x00b0 "degree")         ;U+00B0 DEGREE SIGN
(define-gtkcode #x00b1 "plusminus")      ;U+00B1 PLUS-MINUS SIGN
(define-gtkcode #x00b2 "twosuperior")    ;U+00B2 SUPERSCRIPT TWO
(define-gtkcode #x00b3 "threesuperior")  ;U+00B3 SUPERSCRIPT THREE
(define-gtkcode #x00b4 "acute")          ;U+00B4 ACUTE ACCENT
(define-gtkcode #x00b5 "mu")             ;U+00B5 MICRO SIGN
(define-gtkcode #x00b6 "paragraph")      ;U+00B6 PILCROW SIGN
(define-gtkcode #x00b7 "periodcentered") ;U+00B7 MIDDLE DOT
(define-gtkcode #x00b8 "cedilla")        ;U+00B8 CEDILLA
(define-gtkcode #x00b9 "onesuperior")    ;U+00B9 SUPERSCRIPT ONE
(define-gtkcode #x00ba "masculine") ;U+00BA MASCULINE ORDINAL INDICATOR
(define-gtkcode #x00bb "guillemotright") ;U+00BB RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
(define-gtkcode #x00bc "onequarter") ;U+00BC VULGAR FRACTION ONE QUARTER
(define-gtkcode #x00bd "onehalf")      ;U+00BD VULGAR FRACTION ONE HALF
(define-gtkcode #x00be "threequarters") ;U+00BE VULGAR FRACTION THREE QUARTERS
(define-gtkcode #x00bf "questiondown")   ;U+00BF INVERTED QUESTION MARK
(define-gtkcode #x00c0 "Agrave") ;U+00C0 LATIN CAPITAL LETTER A WITH GRAVE
(define-gtkcode #x00c1 "Aacute") ;U+00C1 LATIN CAPITAL LETTER A WITH ACUTE
(define-gtkcode #x00c2 "Acircumflex") ;U+00C2 LATIN CAPITAL LETTER A WITH CIRCUMFLEX
(define-gtkcode #x00c3 "Atilde") ;U+00C3 LATIN CAPITAL LETTER A WITH TILDE
(define-gtkcode #x00c4 "Adiaeresis") ;U+00C4 LATIN CAPITAL LETTER A WITH DIAERESIS
(define-gtkcode #x00c5 "Aring") ;U+00C5 LATIN CAPITAL LETTER A WITH RING ABOVE
(define-gtkcode #x00c6 "AE")            ;U+00C6 LATIN CAPITAL LETTER AE
(define-gtkcode #x00c7 "Ccedilla") ;U+00C7 LATIN CAPITAL LETTER C WITH CEDILLA
(define-gtkcode #x00c8 "Egrave") ;U+00C8 LATIN CAPITAL LETTER E WITH GRAVE
(define-gtkcode #x00c9 "Eacute") ;U+00C9 LATIN CAPITAL LETTER E WITH ACUTE
(define-gtkcode #x00ca "Ecircumflex") ;U+00CA LATIN CAPITAL LETTER E WITH CIRCUMFLEX
(define-gtkcode #x00cb "Ediaeresis") ;U+00CB LATIN CAPITAL LETTER E WITH DIAERESIS
(define-gtkcode #x00cc "Igrave") ;U+00CC LATIN CAPITAL LETTER I WITH GRAVE
(define-gtkcode #x00cd "Iacute") ;U+00CD LATIN CAPITAL LETTER I WITH ACUTE
(define-gtkcode #x00ce "Icircumflex") ;U+00CE LATIN CAPITAL LETTER I WITH CIRCUMFLEX
(define-gtkcode #x00cf "Idiaeresis") ;U+00CF LATIN CAPITAL LETTER I WITH DIAERESIS
(define-gtkcode #x00d0 "ETH")          ;U+00D0 LATIN CAPITAL LETTER ETH
(define-gtkcode #x00d0 "Eth")            ;deprecated
(define-gtkcode #x00d1 "Ntilde") ;U+00D1 LATIN CAPITAL LETTER N WITH TILDE
(define-gtkcode #x00d2 "Ograve") ;U+00D2 LATIN CAPITAL LETTER O WITH GRAVE
(define-gtkcode #x00d3 "Oacute") ;U+00D3 LATIN CAPITAL LETTER O WITH ACUTE
(define-gtkcode #x00d4 "Ocircumflex") ;U+00D4 LATIN CAPITAL LETTER O WITH CIRCUMFLEX
(define-gtkcode #x00d5 "Otilde") ;U+00D5 LATIN CAPITAL LETTER O WITH TILDE
(define-gtkcode #x00d6 "Odiaeresis") ;U+00D6 LATIN CAPITAL LETTER O WITH DIAERESIS
(define-gtkcode #x00d7 "multiply")       ;U+00D7 MULTIPLICATION SIGN
(define-gtkcode #x00d8 "Oslash") ;U+00D8 LATIN CAPITAL LETTER O WITH STROKE
(define-gtkcode #x00d8 "Ooblique") ;U+00D8 LATIN CAPITAL LETTER O WITH STROKE
(define-gtkcode #x00d9 "Ugrave") ;U+00D9 LATIN CAPITAL LETTER U WITH GRAVE
(define-gtkcode #x00da "Uacute") ;U+00DA LATIN CAPITAL LETTER U WITH ACUTE
(define-gtkcode #x00db "Ucircumflex") ;U+00DB LATIN CAPITAL LETTER U WITH CIRCUMFLEX
(define-gtkcode #x00dc "Udiaeresis") ;U+00DC LATIN CAPITAL LETTER U WITH DIAERESIS
(define-gtkcode #x00dd "Yacute") ;U+00DD LATIN CAPITAL LETTER Y WITH ACUTE
(define-gtkcode #x00de "THORN")      ;U+00DE LATIN CAPITAL LETTER THORN
(define-gtkcode #x00de "Thorn")          ;deprecated
(define-gtkcode #x00df "ssharp")     ;U+00DF LATIN SMALL LETTER SHARP S
(define-gtkcode #x00e0 "agrave") ;U+00E0 LATIN SMALL LETTER A WITH GRAVE
(define-gtkcode #x00e1 "aacute") ;U+00E1 LATIN SMALL LETTER A WITH ACUTE
(define-gtkcode #x00e2 "acircumflex") ;U+00E2 LATIN SMALL LETTER A WITH CIRCUMFLEX
(define-gtkcode #x00e3 "atilde") ;U+00E3 LATIN SMALL LETTER A WITH TILDE
(define-gtkcode #x00e4 "adiaeresis") ;U+00E4 LATIN SMALL LETTER A WITH DIAERESIS
(define-gtkcode #x00e5 "aring") ;U+00E5 LATIN SMALL LETTER A WITH RING ABOVE
(define-gtkcode #x00e6 "ae")             ;U+00E6 LATIN SMALL LETTER AE
(define-gtkcode #x00e7 "ccedilla") ;U+00E7 LATIN SMALL LETTER C WITH CEDILLA
(define-gtkcode #x00e8 "egrave") ;U+00E8 LATIN SMALL LETTER E WITH GRAVE
(define-gtkcode #x00e9 "eacute") ;U+00E9 LATIN SMALL LETTER E WITH ACUTE
(define-gtkcode #x00ea "ecircumflex") ;U+00EA LATIN SMALL LETTER E WITH CIRCUMFLEX
(define-gtkcode #x00eb "ediaeresis") ;U+00EB LATIN SMALL LETTER E WITH DIAERESIS
(define-gtkcode #x00ec "igrave") ;U+00EC LATIN SMALL LETTER I WITH GRAVE
(define-gtkcode #x00ed "iacute") ;U+00ED LATIN SMALL LETTER I WITH ACUTE
(define-gtkcode #x00ee "icircumflex") ;U+00EE LATIN SMALL LETTER I WITH CIRCUMFLEX
(define-gtkcode #x00ef "idiaeresis") ;U+00EF LATIN SMALL LETTER I WITH DIAERESIS
(define-gtkcode #x00f0 "eth")            ;U+00F0 LATIN SMALL LETTER ETH
(define-gtkcode #x00f1 "ntilde") ;U+00F1 LATIN SMALL LETTER N WITH TILDE
(define-gtkcode #x00f2 "ograve") ;U+00F2 LATIN SMALL LETTER O WITH GRAVE
(define-gtkcode #x00f3 "oacute") ;U+00F3 LATIN SMALL LETTER O WITH ACUTE
(define-gtkcode #x00f4 "ocircumflex") ;U+00F4 LATIN SMALL LETTER O WITH CIRCUMFLEX
(define-gtkcode #x00f5 "otilde") ;U+00F5 LATIN SMALL LETTER O WITH TILDE
(define-gtkcode #x00f6 "odiaeresis") ;U+00F6 LATIN SMALL LETTER O WITH DIAERESIS
(define-gtkcode #x00f7 "division")       ;U+00F7 DIVISION SIGN
(define-gtkcode #x00f8 "oslash") ;U+00F8 LATIN SMALL LETTER O WITH STROKE
(define-gtkcode #x00f8 "ooblique") ;U+00F8 LATIN SMALL LETTER O WITH STROKE
(define-gtkcode #x00f9 "ugrave") ;U+00F9 LATIN SMALL LETTER U WITH GRAVE
(define-gtkcode #x00fa "uacute") ;U+00FA LATIN SMALL LETTER U WITH ACUTE
(define-gtkcode #x00fb "ucircumflex") ;U+00FB LATIN SMALL LETTER U WITH CIRCUMFLEX
(define-gtkcode #x00fc "udiaeresis") ;U+00FC LATIN SMALL LETTER U WITH DIAERESIS
(define-gtkcode #x00fd "yacute") ;U+00FD LATIN SMALL LETTER Y WITH ACUTE
(define-gtkcode #x00fe "thorn")        ;U+00FE LATIN SMALL LETTER THORN
(define-gtkcode #x00ff "ydiaeresis") ;U+00FF LATIN SMALL LETTER Y WITH DIAERESIS
(define-gtkcode #x01a1 "Aogonek") ;U+0104 LATIN CAPITAL LETTER A WITH OGONEK
(define-gtkcode #x01a2 "breve")          ;U+02D8 BREVE
(define-gtkcode #x01a3 "Lstroke") ;U+0141 LATIN CAPITAL LETTER L WITH STROKE
(define-gtkcode #x01a5 "Lcaron") ;U+013D LATIN CAPITAL LETTER L WITH CARON
(define-gtkcode #x01a6 "Sacute") ;U+015A LATIN CAPITAL LETTER S WITH ACUTE
(define-gtkcode #x01a9 "Scaron") ;U+0160 LATIN CAPITAL LETTER S WITH CARON
(define-gtkcode #x01aa "Scedilla") ;U+015E LATIN CAPITAL LETTER S WITH CEDILLA
(define-gtkcode #x01ab "Tcaron") ;U+0164 LATIN CAPITAL LETTER T WITH CARON
(define-gtkcode #x01ac "Zacute") ;U+0179 LATIN CAPITAL LETTER Z WITH ACUTE
(define-gtkcode #x01ae "Zcaron") ;U+017D LATIN CAPITAL LETTER Z WITH CARON
(define-gtkcode #x01af "Zabovedot") ;U+017B LATIN CAPITAL LETTER Z WITH DOT ABOVE
(define-gtkcode #x01b1 "aogonek") ;U+0105 LATIN SMALL LETTER A WITH OGONEK
(define-gtkcode #x01b2 "ogonek")         ;U+02DB OGONEK
(define-gtkcode #x01b3 "lstroke") ;U+0142 LATIN SMALL LETTER L WITH STROKE
(define-gtkcode #x01b5 "lcaron") ;U+013E LATIN SMALL LETTER L WITH CARON
(define-gtkcode #x01b6 "sacute") ;U+015B LATIN SMALL LETTER S WITH ACUTE
(define-gtkcode #x01b7 "caron")          ;U+02C7 CARON
(define-gtkcode #x01b9 "scaron") ;U+0161 LATIN SMALL LETTER S WITH CARON
(define-gtkcode #x01ba "scedilla") ;U+015F LATIN SMALL LETTER S WITH CEDILLA
(define-gtkcode #x01bb "tcaron") ;U+0165 LATIN SMALL LETTER T WITH CARON
(define-gtkcode #x01bc "zacute") ;U+017A LATIN SMALL LETTER Z WITH ACUTE
(define-gtkcode #x01bd "doubleacute")    ;U+02DD DOUBLE ACUTE ACCENT
(define-gtkcode #x01be "zcaron") ;U+017E LATIN SMALL LETTER Z WITH CARON
(define-gtkcode #x01bf "zabovedot") ;U+017C LATIN SMALL LETTER Z WITH DOT ABOVE
(define-gtkcode #x01c0 "Racute") ;U+0154 LATIN CAPITAL LETTER R WITH ACUTE
(define-gtkcode #x01c3 "Abreve") ;U+0102 LATIN CAPITAL LETTER A WITH BREVE
(define-gtkcode #x01c5 "Lacute") ;U+0139 LATIN CAPITAL LETTER L WITH ACUTE
(define-gtkcode #x01c6 "Cacute") ;U+0106 LATIN CAPITAL LETTER C WITH ACUTE
(define-gtkcode #x01c8 "Ccaron") ;U+010C LATIN CAPITAL LETTER C WITH CARON
(define-gtkcode #x01ca "Eogonek") ;U+0118 LATIN CAPITAL LETTER E WITH OGONEK
(define-gtkcode #x01cc "Ecaron") ;U+011A LATIN CAPITAL LETTER E WITH CARON
(define-gtkcode #x01cf "Dcaron") ;U+010E LATIN CAPITAL LETTER D WITH CARON
(define-gtkcode #x01d0 "Dstroke") ;U+0110 LATIN CAPITAL LETTER D WITH STROKE
(define-gtkcode #x01d1 "Nacute") ;U+0143 LATIN CAPITAL LETTER N WITH ACUTE
(define-gtkcode #x01d2 "Ncaron") ;U+0147 LATIN CAPITAL LETTER N WITH CARON
(define-gtkcode #x01d5 "Odoubleacute") ;U+0150 LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
(define-gtkcode #x01d8 "Rcaron") ;U+0158 LATIN CAPITAL LETTER R WITH CARON
(define-gtkcode #x01d9 "Uring") ;U+016E LATIN CAPITAL LETTER U WITH RING ABOVE
(define-gtkcode #x01db "Udoubleacute") ;U+0170 LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
(define-gtkcode #x01de "Tcedilla") ;U+0162 LATIN CAPITAL LETTER T WITH CEDILLA
(define-gtkcode #x01e0 "racute") ;U+0155 LATIN SMALL LETTER R WITH ACUTE
(define-gtkcode #x01e3 "abreve") ;U+0103 LATIN SMALL LETTER A WITH BREVE
(define-gtkcode #x01e5 "lacute") ;U+013A LATIN SMALL LETTER L WITH ACUTE
(define-gtkcode #x01e6 "cacute") ;U+0107 LATIN SMALL LETTER C WITH ACUTE
(define-gtkcode #x01e8 "ccaron") ;U+010D LATIN SMALL LETTER C WITH CARON
(define-gtkcode #x01ea "eogonek") ;U+0119 LATIN SMALL LETTER E WITH OGONEK
(define-gtkcode #x01ec "ecaron") ;U+011B LATIN SMALL LETTER E WITH CARON
(define-gtkcode #x01ef "dcaron") ;U+010F LATIN SMALL LETTER D WITH CARON
(define-gtkcode #x01f0 "dstroke") ;U+0111 LATIN SMALL LETTER D WITH STROKE
(define-gtkcode #x01f1 "nacute") ;U+0144 LATIN SMALL LETTER N WITH ACUTE
(define-gtkcode #x01f2 "ncaron") ;U+0148 LATIN SMALL LETTER N WITH CARON
(define-gtkcode #x01f5 "odoubleacute") ;U+0151 LATIN SMALL LETTER O WITH DOUBLE ACUTE
(define-gtkcode #x01fb "udoubleacute") ;U+0171 LATIN SMALL LETTER U WITH DOUBLE ACUTE
(define-gtkcode #x01f8 "rcaron") ;U+0159 LATIN SMALL LETTER R WITH CARON
(define-gtkcode #x01f9 "uring") ;U+016F LATIN SMALL LETTER U WITH RING ABOVE
(define-gtkcode #x01fe "tcedilla") ;U+0163 LATIN SMALL LETTER T WITH CEDILLA
(define-gtkcode #x01ff "abovedot")       ;U+02D9 DOT ABOVE
(define-gtkcode #x02a1 "Hstroke") ;U+0126 LATIN CAPITAL LETTER H WITH STROKE
(define-gtkcode #x02a6 "Hcircumflex") ;U+0124 LATIN CAPITAL LETTER H WITH CIRCUMFLEX
(define-gtkcode #x02a9 "Iabovedot") ;U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
(define-gtkcode #x02ab "Gbreve") ;U+011E LATIN CAPITAL LETTER G WITH BREVE
(define-gtkcode #x02ac "Jcircumflex") ;U+0134 LATIN CAPITAL LETTER J WITH CIRCUMFLEX
(define-gtkcode #x02b1 "hstroke") ;U+0127 LATIN SMALL LETTER H WITH STROKE
(define-gtkcode #x02b6 "hcircumflex") ;U+0125 LATIN SMALL LETTER H WITH CIRCUMFLEX
(define-gtkcode #x02b9 "idotless") ;U+0131 LATIN SMALL LETTER DOTLESS I
(define-gtkcode #x02bb "gbreve") ;U+011F LATIN SMALL LETTER G WITH BREVE
(define-gtkcode #x02bc "jcircumflex") ;U+0135 LATIN SMALL LETTER J WITH CIRCUMFLEX
(define-gtkcode #x02c5 "Cabovedot") ;U+010A LATIN CAPITAL LETTER C WITH DOT ABOVE
(define-gtkcode #x02c6 "Ccircumflex") ;U+0108 LATIN CAPITAL LETTER C WITH CIRCUMFLEX
(define-gtkcode #x02d5 "Gabovedot") ;U+0120 LATIN CAPITAL LETTER G WITH DOT ABOVE
(define-gtkcode #x02d8 "Gcircumflex") ;U+011C LATIN CAPITAL LETTER G WITH CIRCUMFLEX
(define-gtkcode #x02dd "Ubreve") ;U+016C LATIN CAPITAL LETTER U WITH BREVE
(define-gtkcode #x02de "Scircumflex") ;U+015C LATIN CAPITAL LETTER S WITH CIRCUMFLEX
(define-gtkcode #x02e5 "cabovedot") ;U+010B LATIN SMALL LETTER C WITH DOT ABOVE
(define-gtkcode #x02e6 "ccircumflex") ;U+0109 LATIN SMALL LETTER C WITH CIRCUMFLEX
(define-gtkcode #x02f5 "gabovedot") ;U+0121 LATIN SMALL LETTER G WITH DOT ABOVE
(define-gtkcode #x02f8 "gcircumflex") ;U+011D LATIN SMALL LETTER G WITH CIRCUMFLEX
(define-gtkcode #x02fd "ubreve") ;U+016D LATIN SMALL LETTER U WITH BREVE
(define-gtkcode #x02fe "scircumflex") ;U+015D LATIN SMALL LETTER S WITH CIRCUMFLEX
(define-gtkcode #x03a2 "kra")            ;U+0138 LATIN SMALL LETTER KRA
(define-gtkcode #x03a2 "kappa")          ;deprecated
(define-gtkcode #x03a3 "Rcedilla") ;U+0156 LATIN CAPITAL LETTER R WITH CEDILLA
(define-gtkcode #x03a5 "Itilde") ;U+0128 LATIN CAPITAL LETTER I WITH TILDE
(define-gtkcode #x03a6 "Lcedilla") ;U+013B LATIN CAPITAL LETTER L WITH CEDILLA
(define-gtkcode #x03aa "Emacron") ;U+0112 LATIN CAPITAL LETTER E WITH MACRON
(define-gtkcode #x03ab "Gcedilla") ;U+0122 LATIN CAPITAL LETTER G WITH CEDILLA
(define-gtkcode #x03ac "Tslash") ;U+0166 LATIN CAPITAL LETTER T WITH STROKE
(define-gtkcode #x03b3 "rcedilla") ;U+0157 LATIN SMALL LETTER R WITH CEDILLA
(define-gtkcode #x03b5 "itilde") ;U+0129 LATIN SMALL LETTER I WITH TILDE
(define-gtkcode #x03b6 "lcedilla") ;U+013C LATIN SMALL LETTER L WITH CEDILLA
(define-gtkcode #x03ba "emacron") ;U+0113 LATIN SMALL LETTER E WITH MACRON
(define-gtkcode #x03bb "gcedilla") ;U+0123 LATIN SMALL LETTER G WITH CEDILLA
(define-gtkcode #x03bc "tslash") ;U+0167 LATIN SMALL LETTER T WITH STROKE
(define-gtkcode #x03bd "ENG")          ;U+014A LATIN CAPITAL LETTER ENG
(define-gtkcode #x03bf "eng")            ;U+014B LATIN SMALL LETTER ENG
(define-gtkcode #x03c0 "Amacron") ;U+0100 LATIN CAPITAL LETTER A WITH MACRON
(define-gtkcode #x03c7 "Iogonek") ;U+012E LATIN CAPITAL LETTER I WITH OGONEK
(define-gtkcode #x03cc "Eabovedot") ;U+0116 LATIN CAPITAL LETTER E WITH DOT ABOVE
(define-gtkcode #x03cf "Imacron") ;U+012A LATIN CAPITAL LETTER I WITH MACRON
(define-gtkcode #x03d1 "Ncedilla") ;U+0145 LATIN CAPITAL LETTER N WITH CEDILLA
(define-gtkcode #x03d2 "Omacron") ;U+014C LATIN CAPITAL LETTER O WITH MACRON
(define-gtkcode #x03d3 "Kcedilla") ;U+0136 LATIN CAPITAL LETTER K WITH CEDILLA
(define-gtkcode #x03d9 "Uogonek") ;U+0172 LATIN CAPITAL LETTER U WITH OGONEK
(define-gtkcode #x03dd "Utilde") ;U+0168 LATIN CAPITAL LETTER U WITH TILDE
(define-gtkcode #x03de "Umacron") ;U+016A LATIN CAPITAL LETTER U WITH MACRON
(define-gtkcode #x03e0 "amacron") ;U+0101 LATIN SMALL LETTER A WITH MACRON
(define-gtkcode #x03e7 "iogonek") ;U+012F LATIN SMALL LETTER I WITH OGONEK
(define-gtkcode #x03ec "eabovedot") ;U+0117 LATIN SMALL LETTER E WITH DOT ABOVE
(define-gtkcode #x03ef "imacron") ;U+012B LATIN SMALL LETTER I WITH MACRON
(define-gtkcode #x03f1 "ncedilla") ;U+0146 LATIN SMALL LETTER N WITH CEDILLA
(define-gtkcode #x03f2 "omacron") ;U+014D LATIN SMALL LETTER O WITH MACRON
(define-gtkcode #x03f3 "kcedilla") ;U+0137 LATIN SMALL LETTER K WITH CEDILLA
(define-gtkcode #x03f9 "uogonek") ;U+0173 LATIN SMALL LETTER U WITH OGONEK
(define-gtkcode #x03fd "utilde") ;U+0169 LATIN SMALL LETTER U WITH TILDE
(define-gtkcode #x03fe "umacron") ;U+016B LATIN SMALL LETTER U WITH MACRON
(define-gtkcode #x1001e02 "Babovedot") ;U+1E02 LATIN CAPITAL LETTER B WITH DOT ABOVE
(define-gtkcode #x1001e03 "babovedot") ;U+1E03 LATIN SMALL LETTER B WITH DOT ABOVE
(define-gtkcode #x1001e0a "Dabovedot") ;U+1E0A LATIN CAPITAL LETTER D WITH DOT ABOVE
(define-gtkcode #x1001e80 "Wgrave") ;U+1E80 LATIN CAPITAL LETTER W WITH GRAVE
(define-gtkcode #x1001e82 "Wacute") ;U+1E82 LATIN CAPITAL LETTER W WITH ACUTE
(define-gtkcode #x1001e0b "dabovedot") ;U+1E0B LATIN SMALL LETTER D WITH DOT ABOVE
(define-gtkcode #x1001ef2 "Ygrave") ;U+1EF2 LATIN CAPITAL LETTER Y WITH GRAVE
(define-gtkcode #x1001e1e "Fabovedot") ;U+1E1E LATIN CAPITAL LETTER F WITH DOT ABOVE
(define-gtkcode #x1001e1f "fabovedot") ;U+1E1F LATIN SMALL LETTER F WITH DOT ABOVE
(define-gtkcode #x1001e40 "Mabovedot") ;U+1E40 LATIN CAPITAL LETTER M WITH DOT ABOVE
(define-gtkcode #x1001e41 "mabovedot") ;U+1E41 LATIN SMALL LETTER M WITH DOT ABOVE
(define-gtkcode #x1001e56 "Pabovedot") ;U+1E56 LATIN CAPITAL LETTER P WITH DOT ABOVE
(define-gtkcode #x1001e81 "wgrave") ;U+1E81 LATIN SMALL LETTER W WITH GRAVE
(define-gtkcode #x1001e57 "pabovedot") ;U+1E57 LATIN SMALL LETTER P WITH DOT ABOVE
(define-gtkcode #x1001e83 "wacute") ;U+1E83 LATIN SMALL LETTER W WITH ACUTE
(define-gtkcode #x1001e60 "Sabovedot") ;U+1E60 LATIN CAPITAL LETTER S WITH DOT ABOVE
(define-gtkcode #x1001ef3 "ygrave") ;U+1EF3 LATIN SMALL LETTER Y WITH GRAVE
(define-gtkcode #x1001e84 "Wdiaeresis") ;U+1E84 LATIN CAPITAL LETTER W WITH DIAERESIS
(define-gtkcode #x1001e85 "wdiaeresis") ;U+1E85 LATIN SMALL LETTER W WITH DIAERESIS
(define-gtkcode #x1001e61 "sabovedot") ;U+1E61 LATIN SMALL LETTER S WITH DOT ABOVE
(define-gtkcode #x1000174 "Wcircumflex") ;U+0174 LATIN CAPITAL LETTER W WITH CIRCUMFLEX
(define-gtkcode #x1001e6a "Tabovedot") ;U+1E6A LATIN CAPITAL LETTER T WITH DOT ABOVE
(define-gtkcode #x1000176 "Ycircumflex") ;U+0176 LATIN CAPITAL LETTER Y WITH CIRCUMFLEX
(define-gtkcode #x1000175 "wcircumflex") ;U+0175 LATIN SMALL LETTER W WITH CIRCUMFLEX
(define-gtkcode #x1001e6b "tabovedot") ;U+1E6B LATIN SMALL LETTER T WITH DOT ABOVE
(define-gtkcode #x1000177 "ycircumflex") ;U+0177 LATIN SMALL LETTER Y WITH CIRCUMFLEX
(define-gtkcode #x13bc "OE")          ;U+0152 LATIN CAPITAL LIGATURE OE
(define-gtkcode #x13bd "oe")            ;U+0153 LATIN SMALL LIGATURE OE
(define-gtkcode #x13be "Ydiaeresis") ;U+0178 LATIN CAPITAL LETTER Y WITH DIAERESIS
(define-gtkcode #x047e "overline")       ;U+203E OVERLINE
(define-gtkcode #x04a1 "kana_fullstop")  ;U+3002 IDEOGRAPHIC FULL STOP
(define-gtkcode #x04a2 "kana_openingbracket") ;U+300C LEFT CORNER BRACKET
(define-gtkcode #x04a3 "kana_closingbracket") ;U+300D RIGHT CORNER BRACKET
(define-gtkcode #x04a4 "kana_comma")     ;U+3001 IDEOGRAPHIC COMMA
(define-gtkcode #x04a5 "kana_conjunctive") ;U+30FB KATAKANA MIDDLE DOT
(define-gtkcode #x04a5 "kana_middledot") ;deprecated
(define-gtkcode #x04a6 "kana_WO")        ;U+30F2 KATAKANA LETTER WO
(define-gtkcode #x04a7 "kana_a")        ;U+30A1 KATAKANA LETTER SMALL A
(define-gtkcode #x04a8 "kana_i")        ;U+30A3 KATAKANA LETTER SMALL I
(define-gtkcode #x04a9 "kana_u")        ;U+30A5 KATAKANA LETTER SMALL U
(define-gtkcode #x04aa "kana_e")        ;U+30A7 KATAKANA LETTER SMALL E
(define-gtkcode #x04ab "kana_o")        ;U+30A9 KATAKANA LETTER SMALL O
(define-gtkcode #x04ac "kana_ya")      ;U+30E3 KATAKANA LETTER SMALL YA
(define-gtkcode #x04ad "kana_yu")      ;U+30E5 KATAKANA LETTER SMALL YU
(define-gtkcode #x04ae "kana_yo")      ;U+30E7 KATAKANA LETTER SMALL YO
(define-gtkcode #x04af "kana_tsu")     ;U+30C3 KATAKANA LETTER SMALL TU
(define-gtkcode #x04af "kana_tu")        ;deprecated
(define-gtkcode #x04b0 "prolongedsound") ;U+30FC KATAKANA-HIRAGANA PROLONGED SOUND MARK
(define-gtkcode #x04b1 "kana_A")         ;U+30A2 KATAKANA LETTER A
(define-gtkcode #x04b2 "kana_I")         ;U+30A4 KATAKANA LETTER I
(define-gtkcode #x04b3 "kana_U")         ;U+30A6 KATAKANA LETTER U
(define-gtkcode #x04b4 "kana_E")         ;U+30A8 KATAKANA LETTER E
(define-gtkcode #x04b5 "kana_O")         ;U+30AA KATAKANA LETTER O
(define-gtkcode #x04b6 "kana_KA")        ;U+30AB KATAKANA LETTER KA
(define-gtkcode #x04b7 "kana_KI")        ;U+30AD KATAKANA LETTER KI
(define-gtkcode #x04b8 "kana_KU")        ;U+30AF KATAKANA LETTER KU
(define-gtkcode #x04b9 "kana_KE")        ;U+30B1 KATAKANA LETTER KE
(define-gtkcode #x04ba "kana_KO")        ;U+30B3 KATAKANA LETTER KO
(define-gtkcode #x04bb "kana_SA")        ;U+30B5 KATAKANA LETTER SA
(define-gtkcode #x04bc "kana_SHI")       ;U+30B7 KATAKANA LETTER SI
(define-gtkcode #x04bd "kana_SU")        ;U+30B9 KATAKANA LETTER SU
(define-gtkcode #x04be "kana_SE")        ;U+30BB KATAKANA LETTER SE
(define-gtkcode #x04bf "kana_SO")        ;U+30BD KATAKANA LETTER SO
(define-gtkcode #x04c0 "kana_TA")        ;U+30BF KATAKANA LETTER TA
(define-gtkcode #x04c1 "kana_CHI")       ;U+30C1 KATAKANA LETTER TI
(define-gtkcode #x04c1 "kana_TI")        ;deprecated
(define-gtkcode #x04c2 "kana_TSU")       ;U+30C4 KATAKANA LETTER TU
(define-gtkcode #x04c2 "kana_TU")        ;deprecated
(define-gtkcode #x04c3 "kana_TE")        ;U+30C6 KATAKANA LETTER TE
(define-gtkcode #x04c4 "kana_TO")        ;U+30C8 KATAKANA LETTER TO
(define-gtkcode #x04c5 "kana_NA")        ;U+30CA KATAKANA LETTER NA
(define-gtkcode #x04c6 "kana_NI")        ;U+30CB KATAKANA LETTER NI
(define-gtkcode #x04c7 "kana_NU")        ;U+30CC KATAKANA LETTER NU
(define-gtkcode #x04c8 "kana_NE")        ;U+30CD KATAKANA LETTER NE
(define-gtkcode #x04c9 "kana_NO")        ;U+30CE KATAKANA LETTER NO
(define-gtkcode #x04ca "kana_HA")        ;U+30CF KATAKANA LETTER HA
(define-gtkcode #x04cb "kana_HI")        ;U+30D2 KATAKANA LETTER HI
(define-gtkcode #x04cc "kana_FU")        ;U+30D5 KATAKANA LETTER HU
(define-gtkcode #x04cc "kana_HU")        ;deprecated
(define-gtkcode #x04cd "kana_HE")        ;U+30D8 KATAKANA LETTER HE
(define-gtkcode #x04ce "kana_HO")        ;U+30DB KATAKANA LETTER HO
(define-gtkcode #x04cf "kana_MA")        ;U+30DE KATAKANA LETTER MA
(define-gtkcode #x04d0 "kana_MI")        ;U+30DF KATAKANA LETTER MI
(define-gtkcode #x04d1 "kana_MU")        ;U+30E0 KATAKANA LETTER MU
(define-gtkcode #x04d2 "kana_ME")        ;U+30E1 KATAKANA LETTER ME
(define-gtkcode #x04d3 "kana_MO")        ;U+30E2 KATAKANA LETTER MO
(define-gtkcode #x04d4 "kana_YA")        ;U+30E4 KATAKANA LETTER YA
(define-gtkcode #x04d5 "kana_YU")        ;U+30E6 KATAKANA LETTER YU
(define-gtkcode #x04d6 "kana_YO")        ;U+30E8 KATAKANA LETTER YO
(define-gtkcode #x04d7 "kana_RA")        ;U+30E9 KATAKANA LETTER RA
(define-gtkcode #x04d8 "kana_RI")        ;U+30EA KATAKANA LETTER RI
(define-gtkcode #x04d9 "kana_RU")        ;U+30EB KATAKANA LETTER RU
(define-gtkcode #x04da "kana_RE")        ;U+30EC KATAKANA LETTER RE
(define-gtkcode #x04db "kana_RO")        ;U+30ED KATAKANA LETTER RO
(define-gtkcode #x04dc "kana_WA")        ;U+30EF KATAKANA LETTER WA
(define-gtkcode #x04dd "kana_N")         ;U+30F3 KATAKANA LETTER N
(define-gtkcode #x04de "voicedsound") ;U+309B KATAKANA-HIRAGANA VOICED SOUND MARK
(define-gtkcode #x04df "semivoicedsound") ;U+309C KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
(define-gtkcode #xff7e "kana_switch")    ;Alias for mode_switch
(define-gtkcode #x10006f0 "Farsi_0") ;U+06F0 EXTENDED ARABIC-INDIC DIGIT ZERO
(define-gtkcode #x10006f1 "Farsi_1") ;U+06F1 EXTENDED ARABIC-INDIC DIGIT ONE
(define-gtkcode #x10006f2 "Farsi_2") ;U+06F2 EXTENDED ARABIC-INDIC DIGIT TWO
(define-gtkcode #x10006f3 "Farsi_3") ;U+06F3 EXTENDED ARABIC-INDIC DIGIT THREE
(define-gtkcode #x10006f4 "Farsi_4") ;U+06F4 EXTENDED ARABIC-INDIC DIGIT FOUR
(define-gtkcode #x10006f5 "Farsi_5") ;U+06F5 EXTENDED ARABIC-INDIC DIGIT FIVE
(define-gtkcode #x10006f6 "Farsi_6") ;U+06F6 EXTENDED ARABIC-INDIC DIGIT SIX
(define-gtkcode #x10006f7 "Farsi_7") ;U+06F7 EXTENDED ARABIC-INDIC DIGIT SEVEN
(define-gtkcode #x10006f8 "Farsi_8") ;U+06F8 EXTENDED ARABIC-INDIC DIGIT EIGHT
(define-gtkcode #x10006f9 "Farsi_9") ;U+06F9 EXTENDED ARABIC-INDIC DIGIT NINE
(define-gtkcode #x100066a "Arabic_percent") ;U+066A ARABIC PERCENT SIGN
(define-gtkcode #x1000670 "Arabic_superscript_alef") ;U+0670 ARABIC LETTER SUPERSCRIPT ALEF
(define-gtkcode #x1000679 "Arabic_tteh") ;U+0679 ARABIC LETTER TTEH
(define-gtkcode #x100067e "Arabic_peh")  ;U+067E ARABIC LETTER PEH
(define-gtkcode #x1000686 "Arabic_tcheh") ;U+0686 ARABIC LETTER TCHEH
(define-gtkcode #x1000688 "Arabic_ddal") ;U+0688 ARABIC LETTER DDAL
(define-gtkcode #x1000691 "Arabic_rreh") ;U+0691 ARABIC LETTER RREH
(define-gtkcode #x05ac "Arabic_comma")   ;U+060C ARABIC COMMA
(define-gtkcode #x10006d4 "Arabic_fullstop") ;U+06D4 ARABIC FULL STOP
(define-gtkcode #x1000660 "Arabic_0")   ;U+0660 ARABIC-INDIC DIGIT ZERO
(define-gtkcode #x1000661 "Arabic_1")    ;U+0661 ARABIC-INDIC DIGIT ONE
(define-gtkcode #x1000662 "Arabic_2")    ;U+0662 ARABIC-INDIC DIGIT TWO
(define-gtkcode #x1000663 "Arabic_3")  ;U+0663 ARABIC-INDIC DIGIT THREE
(define-gtkcode #x1000664 "Arabic_4")   ;U+0664 ARABIC-INDIC DIGIT FOUR
(define-gtkcode #x1000665 "Arabic_5")   ;U+0665 ARABIC-INDIC DIGIT FIVE
(define-gtkcode #x1000666 "Arabic_6")    ;U+0666 ARABIC-INDIC DIGIT SIX
(define-gtkcode #x1000667 "Arabic_7")  ;U+0667 ARABIC-INDIC DIGIT SEVEN
(define-gtkcode #x1000668 "Arabic_8")  ;U+0668 ARABIC-INDIC DIGIT EIGHT
(define-gtkcode #x1000669 "Arabic_9")   ;U+0669 ARABIC-INDIC DIGIT NINE
(define-gtkcode #x05bb "Arabic_semicolon") ;U+061B ARABIC SEMICOLON
(define-gtkcode #x05bf "Arabic_question_mark") ;U+061F ARABIC QUESTION MARK
(define-gtkcode #x05c1 "Arabic_hamza")   ;U+0621 ARABIC LETTER HAMZA
(define-gtkcode #x05c2 "Arabic_maddaonalef") ;U+0622 ARABIC LETTER ALEF WITH MADDA ABOVE
(define-gtkcode #x05c3 "Arabic_hamzaonalef") ;U+0623 ARABIC LETTER ALEF WITH HAMZA ABOVE
(define-gtkcode #x05c4 "Arabic_hamzaonwaw") ;U+0624 ARABIC LETTER WAW WITH HAMZA ABOVE
(define-gtkcode #x05c5 "Arabic_hamzaunderalef") ;U+0625 ARABIC LETTER ALEF WITH HAMZA BELOW
(define-gtkcode #x05c6 "Arabic_hamzaonyeh") ;U+0626 ARABIC LETTER YEH WITH HAMZA ABOVE
(define-gtkcode #x05c7 "Arabic_alef")    ;U+0627 ARABIC LETTER ALEF
(define-gtkcode #x05c8 "Arabic_beh")     ;U+0628 ARABIC LETTER BEH
(define-gtkcode #x05c9 "Arabic_tehmarbuta") ;U+0629 ARABIC LETTER TEH MARBUTA
(define-gtkcode #x05ca "Arabic_teh")     ;U+062A ARABIC LETTER TEH
(define-gtkcode #x05cb "Arabic_theh")    ;U+062B ARABIC LETTER THEH
(define-gtkcode #x05cc "Arabic_jeem")    ;U+062C ARABIC LETTER JEEM
(define-gtkcode #x05cd "Arabic_hah")     ;U+062D ARABIC LETTER HAH
(define-gtkcode #x05ce "Arabic_khah")    ;U+062E ARABIC LETTER KHAH
(define-gtkcode #x05cf "Arabic_dal")     ;U+062F ARABIC LETTER DAL
(define-gtkcode #x05d0 "Arabic_thal")    ;U+0630 ARABIC LETTER THAL
(define-gtkcode #x05d1 "Arabic_ra")      ;U+0631 ARABIC LETTER REH
(define-gtkcode #x05d2 "Arabic_zain")    ;U+0632 ARABIC LETTER ZAIN
(define-gtkcode #x05d3 "Arabic_seen")    ;U+0633 ARABIC LETTER SEEN
(define-gtkcode #x05d4 "Arabic_sheen")   ;U+0634 ARABIC LETTER SHEEN
(define-gtkcode #x05d5 "Arabic_sad")     ;U+0635 ARABIC LETTER SAD
(define-gtkcode #x05d6 "Arabic_dad")     ;U+0636 ARABIC LETTER DAD
(define-gtkcode #x05d7 "Arabic_tah")     ;U+0637 ARABIC LETTER TAH
(define-gtkcode #x05d8 "Arabic_zah")     ;U+0638 ARABIC LETTER ZAH
(define-gtkcode #x05d9 "Arabic_ain")     ;U+0639 ARABIC LETTER AIN
(define-gtkcode #x05da "Arabic_ghain")   ;U+063A ARABIC LETTER GHAIN
(define-gtkcode #x05e0 "Arabic_tatweel") ;U+0640 ARABIC TATWEEL
(define-gtkcode #x05e1 "Arabic_feh")     ;U+0641 ARABIC LETTER FEH
(define-gtkcode #x05e2 "Arabic_qaf")     ;U+0642 ARABIC LETTER QAF
(define-gtkcode #x05e3 "Arabic_kaf")     ;U+0643 ARABIC LETTER KAF
(define-gtkcode #x05e4 "Arabic_lam")     ;U+0644 ARABIC LETTER LAM
(define-gtkcode #x05e5 "Arabic_meem")    ;U+0645 ARABIC LETTER MEEM
(define-gtkcode #x05e6 "Arabic_noon")    ;U+0646 ARABIC LETTER NOON
(define-gtkcode #x05e7 "Arabic_ha")      ;U+0647 ARABIC LETTER HEH
(define-gtkcode #x05e7 "Arabic_heh")     ;deprecated
(define-gtkcode #x05e8 "Arabic_waw")     ;U+0648 ARABIC LETTER WAW
(define-gtkcode #x05e9 "Arabic_alefmaksura") ;U+0649 ARABIC LETTER ALEF MAKSURA
(define-gtkcode #x05ea "Arabic_yeh")     ;U+064A ARABIC LETTER YEH
(define-gtkcode #x05eb "Arabic_fathatan") ;U+064B ARABIC FATHATAN
(define-gtkcode #x05ec "Arabic_dammatan") ;U+064C ARABIC DAMMATAN
(define-gtkcode #x05ed "Arabic_kasratan") ;U+064D ARABIC KASRATAN
(define-gtkcode #x05ee "Arabic_fatha")   ;U+064E ARABIC FATHA
(define-gtkcode #x05ef "Arabic_damma")   ;U+064F ARABIC DAMMA
(define-gtkcode #x05f0 "Arabic_kasra")   ;U+0650 ARABIC KASRA
(define-gtkcode #x05f1 "Arabic_shadda")  ;U+0651 ARABIC SHADDA
(define-gtkcode #x05f2 "Arabic_sukun")   ;U+0652 ARABIC SUKUN
(define-gtkcode #x1000653 "Arabic_madda_above") ;U+0653 ARABIC MADDAH ABOVE
(define-gtkcode #x1000654 "Arabic_hamza_above") ;U+0654 ARABIC HAMZA ABOVE
(define-gtkcode #x1000655 "Arabic_hamza_below") ;U+0655 ARABIC HAMZA BELOW
(define-gtkcode #x1000698 "Arabic_jeh")  ;U+0698 ARABIC LETTER JEH
(define-gtkcode #x10006a4 "Arabic_veh")  ;U+06A4 ARABIC LETTER VEH
(define-gtkcode #x10006a9 "Arabic_keheh") ;U+06A9 ARABIC LETTER KEHEH
(define-gtkcode #x10006af "Arabic_gaf")  ;U+06AF ARABIC LETTER GAF
(define-gtkcode #x10006ba "Arabic_noon_ghunna") ;U+06BA ARABIC LETTER NOON GHUNNA
(define-gtkcode #x10006be "Arabic_heh_doachashmee") ;U+06BE ARABIC LETTER HEH DOACHASHMEE
(define-gtkcode #x10006cc "Farsi_yeh")  ;U+06CC ARABIC LETTER FARSI YEH
(define-gtkcode #x10006cc "Arabic_farsi_yeh") ;U+06CC ARABIC LETTER FARSI YEH
(define-gtkcode #x10006d2 "Arabic_yeh_baree") ;U+06D2 ARABIC LETTER YEH BARREE
(define-gtkcode #x10006c1 "Arabic_heh_goal") ;U+06C1 ARABIC LETTER HEH GOAL
(define-gtkcode #xff7e "Arabic_switch")  ;Alias for mode_switch
(define-gtkcode #x1000492 "Cyrillic_GHE_bar") ;U+0492 CYRILLIC CAPITAL LETTER GHE WITH STROKE
(define-gtkcode #x100093 "Cyrillic_ghe_bar") ;U+0493 CYRILLIC SMALL LETTER GHE WITH STROKE
(define-gtkcode #x1000496 "Cyrillic_ZHE_descender") ;U+0496 CYRILLIC CAPITAL LETTER ZHE WITH DESCENDER
(define-gtkcode #x1000497 "Cyrillic_zhe_descender") ;U+0497 CYRILLIC SMALL LETTER ZHE WITH DESCENDER
(define-gtkcode #x100049a "Cyrillic_KA_descender") ;U+049A CYRILLIC CAPITAL LETTER KA WITH DESCENDER
(define-gtkcode #x100049b "Cyrillic_ka_descender") ;U+049B CYRILLIC SMALL LETTER KA WITH DESCENDER
(define-gtkcode #x100049c "Cyrillic_KA_vertstroke") ;U+049C CYRILLIC CAPITAL LETTER KA WITH VERTICAL STROKE
(define-gtkcode #x100049d "Cyrillic_ka_vertstroke") ;U+049D CYRILLIC SMALL LETTER KA WITH VERTICAL STROKE
(define-gtkcode #x10004a2 "Cyrillic_EN_descender") ;U+04A2 CYRILLIC CAPITAL LETTER EN WITH DESCENDER
(define-gtkcode #x10004a3 "Cyrillic_en_descender") ;U+04A3 CYRILLIC SMALL LETTER EN WITH DESCENDER
(define-gtkcode #x10004ae "Cyrillic_U_straight") ;U+04AE CYRILLIC CAPITAL LETTER STRAIGHT U
(define-gtkcode #x10004af "Cyrillic_u_straight") ;U+04AF CYRILLIC SMALL LETTER STRAIGHT U
(define-gtkcode #x10004b0 "Cyrillic_U_straight_bar") ;U+04B0 CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE
(define-gtkcode #x10004b1 "Cyrillic_u_straight_bar") ;U+04B1 CYRILLIC SMALL LETTER STRAIGHT U WITH STROKE
(define-gtkcode #x10004b2 "Cyrillic_HA_descender") ;U+04B2 CYRILLIC CAPITAL LETTER HA WITH DESCENDER
(define-gtkcode #x10004b3 "Cyrillic_ha_descender") ;U+04B3 CYRILLIC SMALL LETTER HA WITH DESCENDER
(define-gtkcode #x10004b6 "Cyrillic_CHE_descender") ;U+04B6 CYRILLIC CAPITAL LETTER CHE WITH DESCENDER
(define-gtkcode #x10004b7 "Cyrillic_che_descender") ;U+04B7 CYRILLIC SMALL LETTER CHE WITH DESCENDER
(define-gtkcode #x10004b8 "Cyrillic_CHE_vertstroke") ;U+04B8 CYRILLIC CAPITAL LETTER CHE WITH VERTICAL STROKE
(define-gtkcode #x10004b9 "Cyrillic_che_vertstroke") ;U+04B9 CYRILLIC SMALL LETTER CHE WITH VERTICAL STROKE
(define-gtkcode #x10004ba "Cyrillic_SHHA") ;U+04BA CYRILLIC CAPITAL LETTER SHHA
(define-gtkcode #x10004bb "Cyrillic_shha") ;U+04BB CYRILLIC SMALL LETTER SHHA
(define-gtkcode #x10004d8 "Cyrillic_SCHWA") ;U+04D8 CYRILLIC CAPITAL LETTER SCHWA
(define-gtkcode #x10004d9 "Cyrillic_schwa") ;U+04D9 CYRILLIC SMALL LETTER SCHWA
(define-gtkcode #x10004e2 "Cyrillic_I_macron") ;U+04E2 CYRILLIC CAPITAL LETTER I WITH MACRON
(define-gtkcode #x10004e3 "Cyrillic_i_macron") ;U+04E3 CYRILLIC SMALL LETTER I WITH MACRON
(define-gtkcode #x10004e8 "Cyrillic_O_bar") ;U+04E8 CYRILLIC CAPITAL LETTER BARRED O
(define-gtkcode #x10004e9 "Cyrillic_o_bar") ;U+04E9 CYRILLIC SMALL LETTER BARRED O
(define-gtkcode #x10004ee "Cyrillic_U_macron") ;U+04EE CYRILLIC CAPITAL LETTER U WITH MACRON
(define-gtkcode #x10004ef "Cyrillic_u_macron") ;U+04EF CYRILLIC SMALL LETTER U WITH MACRON
(define-gtkcode #x06a1 "Serbian_dje") ;U+0452 CYRILLIC SMALL LETTER DJE
(define-gtkcode #x06a2 "Macedonia_gje") ;U+0453 CYRILLIC SMALL LETTER GJE
(define-gtkcode #x06a3 "Cyrillic_io")  ;U+0451 CYRILLIC SMALL LETTER IO
(define-gtkcode #x06a4 "Ukrainian_ie") ;U+0454 CYRILLIC SMALL LETTER UKRAINIAN IE
(define-gtkcode #x06a4 "Ukranian_je")    ;deprecated
(define-gtkcode #x06a5 "Macedonia_dse") ;U+0455 CYRILLIC SMALL LETTER DZE
(define-gtkcode #x06a6 "Ukrainian_i") ;U+0456 CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
(define-gtkcode #x06a6 "Ukranian_i")     ;deprecated
(define-gtkcode #x06a7 "Ukrainian_yi") ;U+0457 CYRILLIC SMALL LETTER YI
(define-gtkcode #x06a7 "Ukranian_yi")    ;deprecated
(define-gtkcode #x06a8 "Cyrillic_je")  ;U+0458 CYRILLIC SMALL LETTER JE
(define-gtkcode #x06a8 "Serbian_je")     ;deprecated
(define-gtkcode #x06a9 "Cyrillic_lje") ;U+0459 CYRILLIC SMALL LETTER LJE
(define-gtkcode #x06a9 "Serbian_lje")    ;deprecated
(define-gtkcode #x06aa "Cyrillic_nje") ;U+045A CYRILLIC SMALL LETTER NJE
(define-gtkcode #x06aa "Serbian_nje")    ;deprecated
(define-gtkcode #x06ab "Serbian_tshe") ;U+045B CYRILLIC SMALL LETTER TSHE
(define-gtkcode #x06ac "Macedonia_kje") ;U+045C CYRILLIC SMALL LETTER KJE
(define-gtkcode #x06ad "Ukrainian_ghe_with_upturn") ;U+0491 CYRILLIC SMALL LETTER GHE WITH UPTURN
(define-gtkcode #x06ae "Byelorussian_shortu") ;U+045E CYRILLIC SMALL LETTER SHORT U
(define-gtkcode #x06af "Cyrillic_dzhe") ;U+045F CYRILLIC SMALL LETTER DZHE
(define-gtkcode #x06af "Serbian_dze")    ;deprecated
(define-gtkcode #x06b0 "numerosign")     ;U+2116 NUMERO SIGN
(define-gtkcode #x06b1 "Serbian_DJE") ;U+0402 CYRILLIC CAPITAL LETTER DJE
(define-gtkcode #x06b2 "Macedonia_GJE") ;U+0403 CYRILLIC CAPITAL LETTER GJE
(define-gtkcode #x06b3 "Cyrillic_IO") ;U+0401 CYRILLIC CAPITAL LETTER IO
(define-gtkcode #x06b4 "Ukrainian_IE") ;U+0404 CYRILLIC CAPITAL LETTER UKRAINIAN IE
(define-gtkcode #x06b4 "Ukranian_JE")    ;deprecated
(define-gtkcode #x06b5 "Macedonia_DSE") ;U+0405 CYRILLIC CAPITAL LETTER DZE
(define-gtkcode #x06b6 "Ukrainian_I") ;U+0406 CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
(define-gtkcode #x06b6 "Ukranian_I")     ;deprecated
(define-gtkcode #x06b7 "Ukrainian_YI") ;U+0407 CYRILLIC CAPITAL LETTER YI
(define-gtkcode #x06b7 "Ukranian_YI")    ;deprecated
(define-gtkcode #x06b8 "Cyrillic_JE") ;U+0408 CYRILLIC CAPITAL LETTER JE
(define-gtkcode #x06b8 "Serbian_JE")     ;deprecated
(define-gtkcode #x06b9 "Cyrillic_LJE") ;U+0409 CYRILLIC CAPITAL LETTER LJE
(define-gtkcode #x06b9 "Serbian_LJE")    ;deprecated
(define-gtkcode #x06ba "Cyrillic_NJE") ;U+040A CYRILLIC CAPITAL LETTER NJE
(define-gtkcode #x06ba "Serbian_NJE")    ;deprecated
(define-gtkcode #x06bb "Serbian_TSHE") ;U+040B CYRILLIC CAPITAL LETTER TSHE
(define-gtkcode #x06bc "Macedonia_KJE") ;U+040C CYRILLIC CAPITAL LETTER KJE
(define-gtkcode #x06bd "Ukrainian_GHE_WITH_UPTURN") ;U+0490 CYRILLIC CAPITAL LETTER GHE WITH UPTURN
(define-gtkcode #x06be "Byelorussian_SHORTU") ;U+040E CYRILLIC CAPITAL LETTER SHORT U
(define-gtkcode #x06bf "Cyrillic_DZHE") ;U+040F CYRILLIC CAPITAL LETTER DZHE
(define-gtkcode #x06bf "Serbian_DZE")    ;deprecated
(define-gtkcode #x06c0 "Cyrillic_yu")  ;U+044E CYRILLIC SMALL LETTER YU
(define-gtkcode #x06c1 "Cyrillic_a")    ;U+0430 CYRILLIC SMALL LETTER A
(define-gtkcode #x06c2 "Cyrillic_be")  ;U+0431 CYRILLIC SMALL LETTER BE
(define-gtkcode #x06c3 "Cyrillic_tse") ;U+0446 CYRILLIC SMALL LETTER TSE
(define-gtkcode #x06c4 "Cyrillic_de")  ;U+0434 CYRILLIC SMALL LETTER DE
(define-gtkcode #x06c5 "Cyrillic_ie")  ;U+0435 CYRILLIC SMALL LETTER IE
(define-gtkcode #x06c6 "Cyrillic_ef")  ;U+0444 CYRILLIC SMALL LETTER EF
(define-gtkcode #x06c7 "Cyrillic_ghe") ;U+0433 CYRILLIC SMALL LETTER GHE
(define-gtkcode #x06c8 "Cyrillic_ha")  ;U+0445 CYRILLIC SMALL LETTER HA
(define-gtkcode #x06c9 "Cyrillic_i")    ;U+0438 CYRILLIC SMALL LETTER I
(define-gtkcode #x06ca "Cyrillic_shorti") ;U+0439 CYRILLIC SMALL LETTER SHORT I
(define-gtkcode #x06cb "Cyrillic_ka")  ;U+043A CYRILLIC SMALL LETTER KA
(define-gtkcode #x06cc "Cyrillic_el")  ;U+043B CYRILLIC SMALL LETTER EL
(define-gtkcode #x06cd "Cyrillic_em")  ;U+043C CYRILLIC SMALL LETTER EM
(define-gtkcode #x06ce "Cyrillic_en")  ;U+043D CYRILLIC SMALL LETTER EN
(define-gtkcode #x06cf "Cyrillic_o")    ;U+043E CYRILLIC SMALL LETTER O
(define-gtkcode #x06d0 "Cyrillic_pe")  ;U+043F CYRILLIC SMALL LETTER PE
(define-gtkcode #x06d1 "Cyrillic_ya")  ;U+044F CYRILLIC SMALL LETTER YA
(define-gtkcode #x06d2 "Cyrillic_er")  ;U+0440 CYRILLIC SMALL LETTER ER
(define-gtkcode #x06d3 "Cyrillic_es")  ;U+0441 CYRILLIC SMALL LETTER ES
(define-gtkcode #x06d4 "Cyrillic_te")  ;U+0442 CYRILLIC SMALL LETTER TE
(define-gtkcode #x06d5 "Cyrillic_u")    ;U+0443 CYRILLIC SMALL LETTER U
(define-gtkcode #x06d6 "Cyrillic_zhe") ;U+0436 CYRILLIC SMALL LETTER ZHE
(define-gtkcode #x06d7 "Cyrillic_ve")  ;U+0432 CYRILLIC SMALL LETTER VE
(define-gtkcode #x06d8 "Cyrillic_softsign") ;U+044C CYRILLIC SMALL LETTER SOFT SIGN
(define-gtkcode #x06d9 "Cyrillic_yeru") ;U+044B CYRILLIC SMALL LETTER YERU
(define-gtkcode #x06da "Cyrillic_ze")  ;U+0437 CYRILLIC SMALL LETTER ZE
(define-gtkcode #x06db "Cyrillic_sha") ;U+0448 CYRILLIC SMALL LETTER SHA
(define-gtkcode #x06dc "Cyrillic_e")    ;U+044D CYRILLIC SMALL LETTER E
(define-gtkcode #x06dd "Cyrillic_shcha") ;U+0449 CYRILLIC SMALL LETTER SHCHA
(define-gtkcode #x06de "Cyrillic_che") ;U+0447 CYRILLIC SMALL LETTER CHE
(define-gtkcode #x06df "Cyrillic_hardsign") ;U+044A CYRILLIC SMALL LETTER HARD SIGN
(define-gtkcode #x06e0 "Cyrillic_YU") ;U+042E CYRILLIC CAPITAL LETTER YU
(define-gtkcode #x06e1 "Cyrillic_A")  ;U+0410 CYRILLIC CAPITAL LETTER A
(define-gtkcode #x06e2 "Cyrillic_BE") ;U+0411 CYRILLIC CAPITAL LETTER BE
(define-gtkcode #x06e3 "Cyrillic_TSE") ;U+0426 CYRILLIC CAPITAL LETTER TSE
(define-gtkcode #x06e4 "Cyrillic_DE") ;U+0414 CYRILLIC CAPITAL LETTER DE
(define-gtkcode #x06e5 "Cyrillic_IE") ;U+0415 CYRILLIC CAPITAL LETTER IE
(define-gtkcode #x06e6 "Cyrillic_EF") ;U+0424 CYRILLIC CAPITAL LETTER EF
(define-gtkcode #x06e7 "Cyrillic_GHE") ;U+0413 CYRILLIC CAPITAL LETTER GHE
(define-gtkcode #x06e8 "Cyrillic_HA") ;U+0425 CYRILLIC CAPITAL LETTER HA
(define-gtkcode #x06e9 "Cyrillic_I")  ;U+0418 CYRILLIC CAPITAL LETTER I
(define-gtkcode #x06ea "Cyrillic_SHORTI") ;U+0419 CYRILLIC CAPITAL LETTER SHORT I
(define-gtkcode #x06eb "Cyrillic_KA") ;U+041A CYRILLIC CAPITAL LETTER KA
(define-gtkcode #x06ec "Cyrillic_EL") ;U+041B CYRILLIC CAPITAL LETTER EL
(define-gtkcode #x06ed "Cyrillic_EM") ;U+041C CYRILLIC CAPITAL LETTER EM
(define-gtkcode #x06ee "Cyrillic_EN") ;U+041D CYRILLIC CAPITAL LETTER EN
(define-gtkcode #x06ef "Cyrillic_O")  ;U+041E CYRILLIC CAPITAL LETTER O
(define-gtkcode #x06f0 "Cyrillic_PE") ;U+041F CYRILLIC CAPITAL LETTER PE
(define-gtkcode #x06f1 "Cyrillic_YA") ;U+042F CYRILLIC CAPITAL LETTER YA
(define-gtkcode #x06f2 "Cyrillic_ER") ;U+0420 CYRILLIC CAPITAL LETTER ER
(define-gtkcode #x06f3 "Cyrillic_ES") ;U+0421 CYRILLIC CAPITAL LETTER ES
(define-gtkcode #x06f4 "Cyrillic_TE") ;U+0422 CYRILLIC CAPITAL LETTER TE
(define-gtkcode #x06f5 "Cyrillic_U")  ;U+0423 CYRILLIC CAPITAL LETTER U
(define-gtkcode #x06f6 "Cyrillic_ZHE") ;U+0416 CYRILLIC CAPITAL LETTER ZHE
(define-gtkcode #x06f7 "Cyrillic_VE") ;U+0412 CYRILLIC CAPITAL LETTER VE
(define-gtkcode #x06f8 "Cyrillic_SOFTSIGN") ;U+042C CYRILLIC CAPITAL LETTER SOFT SIGN
(define-gtkcode #x06f9 "Cyrillic_YERU") ;U+042B CYRILLIC CAPITAL LETTER YERU
(define-gtkcode #x06fa "Cyrillic_ZE") ;U+0417 CYRILLIC CAPITAL LETTER ZE
(define-gtkcode #x06fb "Cyrillic_SHA") ;U+0428 CYRILLIC CAPITAL LETTER SHA
(define-gtkcode #x06fc "Cyrillic_E")  ;U+042D CYRILLIC CAPITAL LETTER E
(define-gtkcode #x06fd "Cyrillic_SHCHA") ;U+0429 CYRILLIC CAPITAL LETTER SHCHA
(define-gtkcode #x06fe "Cyrillic_CHE") ;U+0427 CYRILLIC CAPITAL LETTER CHE
(define-gtkcode #x06ff "Cyrillic_HARDSIGN") ;U+042A CYRILLIC CAPITAL LETTER HARD SIGN
(define-gtkcode #x07a1 "Greek_ALPHAaccent") ;U+0386 GREEK CAPITAL LETTER ALPHA WITH TONOS
(define-gtkcode #x07a2 "Greek_EPSILONaccent") ;U+0388 GREEK CAPITAL LETTER EPSILON WITH TONOS
(define-gtkcode #x07a3 "Greek_ETAaccent") ;U+0389 GREEK CAPITAL LETTER ETA WITH TONOS
(define-gtkcode #x07a4 "Greek_IOTAaccent") ;U+038A GREEK CAPITAL LETTER IOTA WITH TONOS
(define-gtkcode #x07a5 "Greek_IOTAdieresis") ;U+03AA GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
(define-gtkcode #x07a5 "Greek_IOTAdiaeresis") ;old typo
(define-gtkcode #x07a7 "Greek_OMICRONaccent") ;U+038C GREEK CAPITAL LETTER OMICRON WITH TONOS
(define-gtkcode #x07a8 "Greek_UPSILONaccent") ;U+038E GREEK CAPITAL LETTER UPSILON WITH TONOS
(define-gtkcode #x07a9 "Greek_UPSILONdieresis") ;U+03AB GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
(define-gtkcode #x07ab "Greek_OMEGAaccent") ;U+038F GREEK CAPITAL LETTER OMEGA WITH TONOS
(define-gtkcode #x07ae "Greek_accentdieresis") ;U+0385 GREEK DIALYTIKA TONOS
(define-gtkcode #x07af "Greek_horizbar") ;U+2015 HORIZONTAL BAR
(define-gtkcode #x07b1 "Greek_alphaaccent") ;U+03AC GREEK SMALL LETTER ALPHA WITH TONOS
(define-gtkcode #x07b2 "Greek_epsilonaccent") ;U+03AD GREEK SMALL LETTER EPSILON WITH TONOS
(define-gtkcode #x07b3 "Greek_etaaccent") ;U+03AE GREEK SMALL LETTER ETA WITH TONOS
(define-gtkcode #x07b4 "Greek_iotaaccent") ;U+03AF GREEK SMALL LETTER IOTA WITH TONOS
(define-gtkcode #x07b5 "Greek_iotadieresis") ;U+03CA GREEK SMALL LETTER IOTA WITH DIALYTIKA
(define-gtkcode #x07b6 "Greek_iotaaccentdieresis") ;U+0390 GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
(define-gtkcode #x07b7 "Greek_omicronaccent") ;U+03CC GREEK SMALL LETTER OMICRON WITH TONOS
(define-gtkcode #x07b8 "Greek_upsilonaccent") ;U+03CD GREEK SMALL LETTER UPSILON WITH TONOS
(define-gtkcode #x07b9 "Greek_upsilondieresis") ;U+03CB GREEK SMALL LETTER UPSILON WITH DIALYTIKA
(define-gtkcode #x07ba "Greek_upsilonaccentdieresis") ;U+03B0 GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
(define-gtkcode #x07bb "Greek_omegaaccent") ;U+03CE GREEK SMALL LETTER OMEGA WITH TONOS
(define-gtkcode #x07c1 "Greek_ALPHA") ;U+0391 GREEK CAPITAL LETTER ALPHA
(define-gtkcode #x07c2 "Greek_BETA")  ;U+0392 GREEK CAPITAL LETTER BETA
(define-gtkcode #x07c3 "Greek_GAMMA") ;U+0393 GREEK CAPITAL LETTER GAMMA
(define-gtkcode #x07c4 "Greek_DELTA") ;U+0394 GREEK CAPITAL LETTER DELTA
(define-gtkcode #x07c5 "Greek_EPSILON") ;U+0395 GREEK CAPITAL LETTER EPSILON
(define-gtkcode #x07c6 "Greek_ZETA")  ;U+0396 GREEK CAPITAL LETTER ZETA
(define-gtkcode #x07c7 "Greek_ETA")    ;U+0397 GREEK CAPITAL LETTER ETA
(define-gtkcode #x07c8 "Greek_THETA") ;U+0398 GREEK CAPITAL LETTER THETA
(define-gtkcode #x07c9 "Greek_IOTA")  ;U+0399 GREEK CAPITAL LETTER IOTA
(define-gtkcode #x07ca "Greek_KAPPA") ;U+039A GREEK CAPITAL LETTER KAPPA
(define-gtkcode #x07cb "Greek_LAMDA") ;U+039B GREEK CAPITAL LETTER LAMDA
(define-gtkcode #x07cb "Greek_LAMBDA") ;U+039B GREEK CAPITAL LETTER LAMDA
(define-gtkcode #x07cc "Greek_MU")      ;U+039C GREEK CAPITAL LETTER MU
(define-gtkcode #x07cd "Greek_NU")      ;U+039D GREEK CAPITAL LETTER NU
(define-gtkcode #x07ce "Greek_XI")      ;U+039E GREEK CAPITAL LETTER XI
(define-gtkcode #x07cf "Greek_OMICRON") ;U+039F GREEK CAPITAL LETTER OMICRON
(define-gtkcode #x07d0 "Greek_PI")      ;U+03A0 GREEK CAPITAL LETTER PI
(define-gtkcode #x07d1 "Greek_RHO")    ;U+03A1 GREEK CAPITAL LETTER RHO
(define-gtkcode #x07d2 "Greek_SIGMA") ;U+03A3 GREEK CAPITAL LETTER SIGMA
(define-gtkcode #x07d4 "Greek_TAU")    ;U+03A4 GREEK CAPITAL LETTER TAU
(define-gtkcode #x07d5 "Greek_UPSILON") ;U+03A5 GREEK CAPITAL LETTER UPSILON
(define-gtkcode #x07d6 "Greek_PHI")    ;U+03A6 GREEK CAPITAL LETTER PHI
(define-gtkcode #x07d7 "Greek_CHI")    ;U+03A7 GREEK CAPITAL LETTER CHI
(define-gtkcode #x07d8 "Greek_PSI")    ;U+03A8 GREEK CAPITAL LETTER PSI
(define-gtkcode #x07d9 "Greek_OMEGA") ;U+03A9 GREEK CAPITAL LETTER OMEGA
(define-gtkcode #x07e1 "Greek_alpha")  ;U+03B1 GREEK SMALL LETTER ALPHA
(define-gtkcode #x07e2 "Greek_beta")    ;U+03B2 GREEK SMALL LETTER BETA
(define-gtkcode #x07e3 "Greek_gamma")  ;U+03B3 GREEK SMALL LETTER GAMMA
(define-gtkcode #x07e4 "Greek_delta")  ;U+03B4 GREEK SMALL LETTER DELTA
(define-gtkcode #x07e5 "Greek_epsilon") ;U+03B5 GREEK SMALL LETTER EPSILON
(define-gtkcode #x07e6 "Greek_zeta")    ;U+03B6 GREEK SMALL LETTER ZETA
(define-gtkcode #x07e7 "Greek_eta")      ;U+03B7 GREEK SMALL LETTER ETA
(define-gtkcode #x07e8 "Greek_theta")  ;U+03B8 GREEK SMALL LETTER THETA
(define-gtkcode #x07e9 "Greek_iota")    ;U+03B9 GREEK SMALL LETTER IOTA
(define-gtkcode #x07ea "Greek_kappa")  ;U+03BA GREEK SMALL LETTER KAPPA
(define-gtkcode #x07eb "Greek_lamda")  ;U+03BB GREEK SMALL LETTER LAMDA
(define-gtkcode #x07eb "Greek_lambda") ;U+03BB GREEK SMALL LETTER LAMDA
(define-gtkcode #x07ec "Greek_mu")       ;U+03BC GREEK SMALL LETTER MU
(define-gtkcode #x07ed "Greek_nu")       ;U+03BD GREEK SMALL LETTER NU
(define-gtkcode #x07ee "Greek_xi")       ;U+03BE GREEK SMALL LETTER XI
(define-gtkcode #x07ef "Greek_omicron") ;U+03BF GREEK SMALL LETTER OMICRON
(define-gtkcode #x07f0 "Greek_pi")       ;U+03C0 GREEK SMALL LETTER PI
(define-gtkcode #x07f1 "Greek_rho")      ;U+03C1 GREEK SMALL LETTER RHO
(define-gtkcode #x07f2 "Greek_sigma")  ;U+03C3 GREEK SMALL LETTER SIGMA
(define-gtkcode #x07f3 "Greek_finalsmallsigma") ;U+03C2 GREEK SMALL LETTER FINAL SIGMA
(define-gtkcode #x07f4 "Greek_tau")      ;U+03C4 GREEK SMALL LETTER TAU
(define-gtkcode #x07f5 "Greek_upsilon") ;U+03C5 GREEK SMALL LETTER UPSILON
(define-gtkcode #x07f6 "Greek_phi")      ;U+03C6 GREEK SMALL LETTER PHI
(define-gtkcode #x07f7 "Greek_chi")      ;U+03C7 GREEK SMALL LETTER CHI
(define-gtkcode #x07f8 "Greek_psi")      ;U+03C8 GREEK SMALL LETTER PSI
(define-gtkcode #x07f9 "Greek_omega")  ;U+03C9 GREEK SMALL LETTER OMEGA
(define-gtkcode #xff7e "Greek_switch")   ;Alias for mode_switch
(define-gtkcode #x08a1 "leftradical")    ;U+23B7 RADICAL SYMBOL BOTTOM
(define-gtkcode #x08a2 "topleftradical") ;(U+250C BOX DRAWINGS LIGHT DOWN AND RIGHT)
(define-gtkcode #x08a3 "horizconnector") ;(U+2500 BOX DRAWINGS LIGHT HORIZONTAL)
(define-gtkcode #x08a4 "topintegral")    ;U+2320 TOP HALF INTEGRAL
(define-gtkcode #x08a5 "botintegral")    ;U+2321 BOTTOM HALF INTEGRAL
(define-gtkcode #x08a6 "vertconnector") ;(U+2502 BOX DRAWINGS LIGHT VERTICAL)
(define-gtkcode #x08a7 "topleftsqbracket") ;U+23A1 LEFT SQUARE BRACKET UPPER CORNER
(define-gtkcode #x08a8 "botleftsqbracket") ;U+23A3 LEFT SQUARE BRACKET LOWER CORNER
(define-gtkcode #x08a9 "toprightsqbracket") ;U+23A4 RIGHT SQUARE BRACKET UPPER CORNER
(define-gtkcode #x08aa "botrightsqbracket") ;U+23A6 RIGHT SQUARE BRACKET LOWER CORNER
(define-gtkcode #x08ab "topleftparens") ;U+239B LEFT PARENTHESIS UPPER HOOK
(define-gtkcode #x08ac "botleftparens") ;U+239D LEFT PARENTHESIS LOWER HOOK
(define-gtkcode #x08ad "toprightparens") ;U+239E RIGHT PARENTHESIS UPPER HOOK
(define-gtkcode #x08ae "botrightparens") ;U+23A0 RIGHT PARENTHESIS LOWER HOOK
(define-gtkcode #x08af "leftmiddlecurlybrace") ;U+23A8 LEFT CURLY BRACKET MIDDLE PIECE
(define-gtkcode #x08b0 "rightmiddlecurlybrace") ;U+23AC RIGHT CURLY BRACKET MIDDLE PIECE
(define-gtkcode #x08b1 "topleftsummation")
(define-gtkcode #x08b2 "botleftsummation")
(define-gtkcode #x08b3 "topvertsummationconnector")
(define-gtkcode #x08b4 "botvertsummationconnector")
(define-gtkcode #x08b5 "toprightsummation")
(define-gtkcode #x08b6 "botrightsummation")
(define-gtkcode #x08b7 "rightmiddlesummation")
(define-gtkcode #x08bc "lessthanequal")  ;U+2264 LESS-THAN OR EQUAL TO
(define-gtkcode #x08bd "notequal")       ;U+2260 NOT EQUAL TO
(define-gtkcode #x08be "greaterthanequal") ;U+2265 GREATER-THAN OR EQUAL TO
(define-gtkcode #x08bf "integral")       ;U+222B INTEGRAL
(define-gtkcode #x08c0 "therefore")      ;U+2234 THEREFORE
(define-gtkcode #x08c1 "variation")      ;U+221D PROPORTIONAL TO
(define-gtkcode #x08c2 "infinity")       ;U+221E INFINITY
(define-gtkcode #x08c5 "nabla")          ;U+2207 NABLA
(define-gtkcode #x08c8 "approximate")    ;U+223C TILDE OPERATOR
(define-gtkcode #x08c9 "similarequal")  ;U+2243 ASYMPTOTICALLY EQUAL TO
(define-gtkcode #x08cd "ifonlyif")      ;U+21D4 LEFT RIGHT DOUBLE ARROW
(define-gtkcode #x08ce "implies")       ;U+21D2 RIGHTWARDS DOUBLE ARROW
(define-gtkcode #x08cf "identical")      ;U+2261 IDENTICAL TO
(define-gtkcode #x08d6 "radical")        ;U+221A SQUARE ROOT
(define-gtkcode #x08da "includedin")     ;U+2282 SUBSET OF
(define-gtkcode #x08db "includes")       ;U+2283 SUPERSET OF
(define-gtkcode #x08dc "intersection")   ;U+2229 INTERSECTION
(define-gtkcode #x08dd "union")          ;U+222A UNION
(define-gtkcode #x08de "logicaland")     ;U+2227 LOGICAL AND
(define-gtkcode #x08df "logicalor")      ;U+2228 LOGICAL OR
(define-gtkcode #x08ef "partialderivative") ;U+2202 PARTIAL DIFFERENTIAL
(define-gtkcode #x08f6 "function") ;U+0192 LATIN SMALL LETTER F WITH HOOK
(define-gtkcode #x08fb "leftarrow")      ;U+2190 LEFTWARDS ARROW
(define-gtkcode #x08fc "uparrow")        ;U+2191 UPWARDS ARROW
(define-gtkcode #x08fd "rightarrow")     ;U+2192 RIGHTWARDS ARROW
(define-gtkcode #x08fe "downarrow")      ;U+2193 DOWNWARDS ARROW
(define-gtkcode #x09df "blank")
(define-gtkcode #x09e0 "soliddiamond")   ;U+25C6 BLACK DIAMOND
(define-gtkcode #x09e1 "checkerboard")   ;U+2592 MEDIUM SHADE
(define-gtkcode #x09e2 "ht")   ;U+2409 SYMBOL FOR HORIZONTAL TABULATION
(define-gtkcode #x09e3 "ff")             ;U+240C SYMBOL FOR FORM FEED
(define-gtkcode #x09e4 "cr")         ;U+240D SYMBOL FOR CARRIAGE RETURN
(define-gtkcode #x09e5 "lf")             ;U+240A SYMBOL FOR LINE FEED
(define-gtkcode #x09e8 "nl")             ;U+2424 SYMBOL FOR NEWLINE
(define-gtkcode #x09e9 "vt")     ;U+240B SYMBOL FOR VERTICAL TABULATION
(define-gtkcode #x09ea "lowrightcorner") ;U+2518 BOX DRAWINGS LIGHT UP AND LEFT
(define-gtkcode #x09eb "uprightcorner") ;U+2510 BOX DRAWINGS LIGHT DOWN AND LEFT
(define-gtkcode #x09ec "upleftcorner") ;U+250C BOX DRAWINGS LIGHT DOWN AND RIGHT
(define-gtkcode #x09ed "lowleftcorner") ;U+2514 BOX DRAWINGS LIGHT UP AND RIGHT
(define-gtkcode #x09ee "crossinglines") ;U+253C BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
(define-gtkcode #x09ef "horizlinescan1") ;U+23BA HORIZONTAL SCAN LINE-1
(define-gtkcode #x09f0 "horizlinescan3") ;U+23BB HORIZONTAL SCAN LINE-3
(define-gtkcode #x09f1 "horizlinescan5") ;U+2500 BOX DRAWINGS LIGHT HORIZONTAL
(define-gtkcode #x09f2 "horizlinescan7") ;U+23BC HORIZONTAL SCAN LINE-7
(define-gtkcode #x09f3 "horizlinescan9") ;U+23BD HORIZONTAL SCAN LINE-9
(define-gtkcode #x09f4 "leftt") ;U+251C BOX DRAWINGS LIGHT VERTICAL AND RIGHT
(define-gtkcode #x09f5 "rightt") ;U+2524 BOX DRAWINGS LIGHT VERTICAL AND LEFT
(define-gtkcode #x09f6 "bott") ;U+2534 BOX DRAWINGS LIGHT UP AND HORIZONTAL
(define-gtkcode #x09f7 "topt") ;U+252C BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
(define-gtkcode #x09f8 "vertbar")   ;U+2502 BOX DRAWINGS LIGHT VERTICAL
(define-gtkcode #x0aa1 "emspace")        ;U+2003 EM SPACE
(define-gtkcode #x0aa2 "enspace")        ;U+2002 EN SPACE
(define-gtkcode #x0aa3 "em3space")       ;U+2004 THREE-PER-EM SPACE
(define-gtkcode #x0aa4 "em4space")       ;U+2005 FOUR-PER-EM SPACE
(define-gtkcode #x0aa5 "digitspace")     ;U+2007 FIGURE SPACE
(define-gtkcode #x0aa6 "punctspace")     ;U+2008 PUNCTUATION SPACE
(define-gtkcode #x0aa7 "thinspace")      ;U+2009 THIN SPACE
(define-gtkcode #x0aa8 "hairspace")      ;U+200A HAIR SPACE
(define-gtkcode #x0aa9 "emdash")         ;U+2014 EM DASH
(define-gtkcode #x0aaa "endash")         ;U+2013 EN DASH
(define-gtkcode #x0aac "signifblank")    ;(U+2423 OPEN BOX)
(define-gtkcode #x0aae "ellipsis")       ;U+2026 HORIZONTAL ELLIPSIS
(define-gtkcode #x0aaf "doubbaselinedot") ;U+2025 TWO DOT LEADER
(define-gtkcode #x0ab0 "onethird")    ;U+2153 VULGAR FRACTION ONE THIRD
(define-gtkcode #x0ab1 "twothirds")  ;U+2154 VULGAR FRACTION TWO THIRDS
(define-gtkcode #x0ab2 "onefifth")    ;U+2155 VULGAR FRACTION ONE FIFTH
(define-gtkcode #x0ab3 "twofifths")  ;U+2156 VULGAR FRACTION TWO FIFTHS
(define-gtkcode #x0ab4 "threefifths") ;U+2157 VULGAR FRACTION THREE FIFTHS
(define-gtkcode #x0ab5 "fourfifths") ;U+2158 VULGAR FRACTION FOUR FIFTHS
(define-gtkcode #x0ab6 "onesixth")    ;U+2159 VULGAR FRACTION ONE SIXTH
(define-gtkcode #x0ab7 "fivesixths") ;U+215A VULGAR FRACTION FIVE SIXTHS
(define-gtkcode #x0ab8 "careof")         ;U+2105 CARE OF
(define-gtkcode #x0abb "figdash")        ;U+2012 FIGURE DASH
(define-gtkcode #x0abc "leftanglebracket") ;(U+27E8 MATHEMATICAL LEFT ANGLE BRACKET)
(define-gtkcode #x0abd "decimalpoint")   ;(U+002E FULL STOP)
(define-gtkcode #x0abe "rightanglebracket") ;(U+27E9 MATHEMATICAL RIGHT ANGLE BRACKET)
(define-gtkcode #x0abf "marker")
(define-gtkcode #x0ac3 "oneeighth")  ;U+215B VULGAR FRACTION ONE EIGHTH
(define-gtkcode #x0ac4 "threeeighths") ;U+215C VULGAR FRACTION THREE EIGHTHS
(define-gtkcode #x0ac5 "fiveeighths") ;U+215D VULGAR FRACTION FIVE EIGHTHS
(define-gtkcode #x0ac6 "seveneighths") ;U+215E VULGAR FRACTION SEVEN EIGHTHS
(define-gtkcode #x0ac9 "trademark")      ;U+2122 TRADE MARK SIGN
(define-gtkcode #x0aca "signaturemark")  ;(U+2613 SALTIRE)
(define-gtkcode #x0acb "trademarkincircle")
(define-gtkcode #x0acc "leftopentriangle") ;(U+25C1 WHITE LEFT-POINTING TRIANGLE)
(define-gtkcode #x0acd "rightopentriangle") ;(U+25B7 WHITE RIGHT-POINTING TRIANGLE)
(define-gtkcode #x0ace "emopencircle")   ;(U+25CB WHITE CIRCLE)
(define-gtkcode #x0acf "emopenrectangle") ;(U+25AF WHITE VERTICAL RECTANGLE)
(define-gtkcode #x0ad0 "leftsinglequotemark") ;U+2018 LEFT SINGLE QUOTATION MARK
(define-gtkcode #x0ad1 "rightsinglequotemark") ;U+2019 RIGHT SINGLE QUOTATION MARK
(define-gtkcode #x0ad2 "leftdoublequotemark") ;U+201C LEFT DOUBLE QUOTATION MARK
(define-gtkcode #x0ad3 "rightdoublequotemark") ;U+201D RIGHT DOUBLE QUOTATION MARK
(define-gtkcode #x0ad4 "prescription")   ;U+211E PRESCRIPTION TAKE
(define-gtkcode #x0ad6 "minutes")        ;U+2032 PRIME
(define-gtkcode #x0ad7 "seconds")        ;U+2033 DOUBLE PRIME
(define-gtkcode #x0ad9 "latincross")     ;U+271D LATIN CROSS
(define-gtkcode #x0ada "hexagram")
(define-gtkcode #x0adb "filledrectbullet") ;(U+25AC BLACK RECTANGLE)
(define-gtkcode #x0adc "filledlefttribullet") ;(U+25C0 BLACK LEFT-POINTING TRIANGLE)
(define-gtkcode #x0add "filledrighttribullet") ;(U+25B6 BLACK RIGHT-POINTING TRIANGLE)
(define-gtkcode #x0ade "emfilledcircle") ;(U+25CF BLACK CIRCLE)
(define-gtkcode #x0adf "emfilledrect") ;(U+25AE BLACK VERTICAL RECTANGLE)
(define-gtkcode #x0ae0 "enopencircbullet") ;(U+25E6 WHITE BULLET)
(define-gtkcode #x0ae1 "enopensquarebullet") ;(U+25AB WHITE SMALL SQUARE)
(define-gtkcode #x0ae2 "openrectbullet") ;(U+25AD WHITE RECTANGLE)
(define-gtkcode #x0ae3 "opentribulletup") ;(U+25B3 WHITE UP-POINTING TRIANGLE)
(define-gtkcode #x0ae4 "opentribulletdown") ;(U+25BD WHITE DOWN-POINTING TRIANGLE)
(define-gtkcode #x0ae5 "openstar")       ;(U+2606 WHITE STAR)
(define-gtkcode #x0ae6 "enfilledcircbullet") ;(U+2022 BULLET)
(define-gtkcode #x0ae7 "enfilledsqbullet") ;(U+25AA BLACK SMALL SQUARE)
(define-gtkcode #x0ae8 "filledtribulletup") ;(U+25B2 BLACK UP-POINTING TRIANGLE)
(define-gtkcode #x0ae9 "filledtribulletdown") ;(U+25BC BLACK DOWN-POINTING TRIANGLE)
(define-gtkcode #x0aea "leftpointer") ;(U+261C WHITE LEFT POINTING INDEX)
(define-gtkcode #x0aeb "rightpointer") ;(U+261E WHITE RIGHT POINTING INDEX)
(define-gtkcode #x0aec "club")           ;U+2663 BLACK CLUB SUIT
(define-gtkcode #x0aed "diamond")        ;U+2666 BLACK DIAMOND SUIT
(define-gtkcode #x0aee "heart")          ;U+2665 BLACK HEART SUIT
(define-gtkcode #x0af0 "maltesecross")   ;U+2720 MALTESE CROSS
(define-gtkcode #x0af1 "dagger")         ;U+2020 DAGGER
(define-gtkcode #x0af2 "doubledagger")   ;U+2021 DOUBLE DAGGER
(define-gtkcode #x0af3 "checkmark")      ;U+2713 CHECK MARK
(define-gtkcode #x0af4 "ballotcross")    ;U+2717 BALLOT X
(define-gtkcode #x0af5 "musicalsharp")   ;U+266F MUSIC SHARP SIGN
(define-gtkcode #x0af6 "musicalflat")    ;U+266D MUSIC FLAT SIGN
(define-gtkcode #x0af7 "malesymbol")     ;U+2642 MALE SIGN
(define-gtkcode #x0af8 "femalesymbol")   ;U+2640 FEMALE SIGN
(define-gtkcode #x0af9 "telephone")      ;U+260E BLACK TELEPHONE
(define-gtkcode #x0afa "telephonerecorder") ;U+2315 TELEPHONE RECORDER
(define-gtkcode #x0afb "phonographcopyright") ;U+2117 SOUND RECORDING COPYRIGHT
(define-gtkcode #x0afc "caret")          ;U+2038 CARET
(define-gtkcode #x0afd "singlelowquotemark") ;U+201A SINGLE LOW-9 QUOTATION MARK
(define-gtkcode #x0afe "doublelowquotemark") ;U+201E DOUBLE LOW-9 QUOTATION MARK
(define-gtkcode #x0aff "cursor")
(define-gtkcode #x0ba3 "leftcaret")      ;(U+003C LESS-THAN SIGN)
(define-gtkcode #x0ba6 "rightcaret")     ;(U+003E GREATER-THAN SIGN)
(define-gtkcode #x0ba8 "downcaret")      ;(U+2228 LOGICAL OR)
(define-gtkcode #x0ba9 "upcaret")        ;(U+2227 LOGICAL AND)
(define-gtkcode #x0bc0 "overbar")        ;(U+00AF MACRON)
(define-gtkcode #x0bc2 "downtack")       ;U+22A5 UP TACK
(define-gtkcode #x0bc3 "upshoe")         ;(U+2229 INTERSECTION)
(define-gtkcode #x0bc4 "downstile")      ;U+230A LEFT FLOOR
(define-gtkcode #x0bc6 "underbar")       ;(U+005F LOW LINE)
(define-gtkcode #x0bca "jot")            ;U+2218 RING OPERATOR
(define-gtkcode #x0bcc "quad")       ;U+2395 APL FUNCTIONAL SYMBOL QUAD
(define-gtkcode #x0bce "uptack")         ;U+22A4 DOWN TACK
(define-gtkcode #x0bcf "circle")         ;U+25CB WHITE CIRCLE
(define-gtkcode #x0bd3 "upstile")        ;U+2308 LEFT CEILING
(define-gtkcode #x0bd6 "downshoe")       ;(U+222A UNION)
(define-gtkcode #x0bd8 "rightshoe")      ;(U+2283 SUPERSET OF)
(define-gtkcode #x0bda "leftshoe")       ;(U+2282 SUBSET OF)
(define-gtkcode #x0bdc "lefttack")       ;U+22A2 RIGHT TACK
(define-gtkcode #x0bfc "righttack")      ;U+22A3 LEFT TACK
(define-gtkcode #x0cdf "hebrew_doublelowline") ;U+2017 DOUBLE LOW LINE
(define-gtkcode #x0ce0 "hebrew_aleph")   ;U+05D0 HEBREW LETTER ALEF
(define-gtkcode #x0ce1 "hebrew_bet")     ;U+05D1 HEBREW LETTER BET
(define-gtkcode #x0ce1 "hebrew_beth")    ;deprecated
(define-gtkcode #x0ce2 "hebrew_gimel")   ;U+05D2 HEBREW LETTER GIMEL
(define-gtkcode #x0ce2 "hebrew_gimmel")  ;deprecated
(define-gtkcode #x0ce3 "hebrew_dalet")   ;U+05D3 HEBREW LETTER DALET
(define-gtkcode #x0ce3 "hebrew_daleth")  ;deprecated
(define-gtkcode #x0ce4 "hebrew_he")      ;U+05D4 HEBREW LETTER HE
(define-gtkcode #x0ce5 "hebrew_waw")     ;U+05D5 HEBREW LETTER VAV
(define-gtkcode #x0ce6 "hebrew_zain")    ;U+05D6 HEBREW LETTER ZAYIN
(define-gtkcode #x0ce6 "hebrew_zayin")   ;deprecated
(define-gtkcode #x0ce7 "hebrew_chet")    ;U+05D7 HEBREW LETTER HET
(define-gtkcode #x0ce7 "hebrew_het")     ;deprecated
(define-gtkcode #x0ce8 "hebrew_tet")     ;U+05D8 HEBREW LETTER TET
(define-gtkcode #x0ce8 "hebrew_teth")    ;deprecated
(define-gtkcode #x0ce9 "hebrew_yod")     ;U+05D9 HEBREW LETTER YOD
(define-gtkcode #x0cea "hebrew_finalkaph") ;U+05DA HEBREW LETTER FINAL KAF
(define-gtkcode #x0ceb "hebrew_kaph")    ;U+05DB HEBREW LETTER KAF
(define-gtkcode #x0cec "hebrew_lamed")   ;U+05DC HEBREW LETTER LAMED
(define-gtkcode #x0ced "hebrew_finalmem") ;U+05DD HEBREW LETTER FINAL MEM
(define-gtkcode #x0cee "hebrew_mem")     ;U+05DE HEBREW LETTER MEM
(define-gtkcode #x0cef "hebrew_finalnun") ;U+05DF HEBREW LETTER FINAL NUN
(define-gtkcode #x0cf0 "hebrew_nun")     ;U+05E0 HEBREW LETTER NUN
(define-gtkcode #x0cf1 "hebrew_samech")  ;U+05E1 HEBREW LETTER SAMEKH
(define-gtkcode #x0cf1 "hebrew_samekh")  ;deprecated
(define-gtkcode #x0cf2 "hebrew_ayin")    ;U+05E2 HEBREW LETTER AYIN
(define-gtkcode #x0cf3 "hebrew_finalpe") ;U+05E3 HEBREW LETTER FINAL PE
(define-gtkcode #x0cf4 "hebrew_pe")      ;U+05E4 HEBREW LETTER PE
(define-gtkcode #x0cf5 "hebrew_finalzade") ;U+05E5 HEBREW LETTER FINAL TSADI
(define-gtkcode #x0cf5 "hebrew_finalzadi") ;deprecated
(define-gtkcode #x0cf6 "hebrew_zade")    ;U+05E6 HEBREW LETTER TSADI
(define-gtkcode #x0cf6 "hebrew_zadi")    ;deprecated
(define-gtkcode #x0cf7 "hebrew_qoph")    ;U+05E7 HEBREW LETTER QOF
(define-gtkcode #x0cf7 "hebrew_kuf")     ;deprecated
(define-gtkcode #x0cf8 "hebrew_resh")    ;U+05E8 HEBREW LETTER RESH
(define-gtkcode #x0cf9 "hebrew_shin")    ;U+05E9 HEBREW LETTER SHIN
(define-gtkcode #x0cfa "hebrew_taw")     ;U+05EA HEBREW LETTER TAV
(define-gtkcode #x0cfa "hebrew_taf")     ;deprecated
(define-gtkcode #xff7e "Hebrew_switch")  ;Alias for mode_switch
(define-gtkcode #x0da1 "Thai_kokai")     ;U+0E01 THAI CHARACTER KO KAI
(define-gtkcode #x0da2 "Thai_khokhai")  ;U+0E02 THAI CHARACTER KHO KHAI
(define-gtkcode #x0da3 "Thai_khokhuat") ;U+0E03 THAI CHARACTER KHO KHUAT
(define-gtkcode #x0da4 "Thai_khokhwai") ;U+0E04 THAI CHARACTER KHO KHWAI
(define-gtkcode #x0da5 "Thai_khokhon")  ;U+0E05 THAI CHARACTER KHO KHON
(define-gtkcode #x0da6 "Thai_khorakhang") ;U+0E06 THAI CHARACTER KHO RAKHANG
(define-gtkcode #x0da7 "Thai_ngongu")    ;U+0E07 THAI CHARACTER NGO NGU
(define-gtkcode #x0da8 "Thai_chochan")  ;U+0E08 THAI CHARACTER CHO CHAN
(define-gtkcode #x0da9 "Thai_choching") ;U+0E09 THAI CHARACTER CHO CHING
(define-gtkcode #x0daa "Thai_chochang") ;U+0E0A THAI CHARACTER CHO CHANG
(define-gtkcode #x0dab "Thai_soso")      ;U+0E0B THAI CHARACTER SO SO
(define-gtkcode #x0dac "Thai_chochoe")  ;U+0E0C THAI CHARACTER CHO CHOE
(define-gtkcode #x0dad "Thai_yoying")    ;U+0E0D THAI CHARACTER YO YING
(define-gtkcode #x0dae "Thai_dochada")  ;U+0E0E THAI CHARACTER DO CHADA
(define-gtkcode #x0daf "Thai_topatak")  ;U+0E0F THAI CHARACTER TO PATAK
(define-gtkcode #x0db0 "Thai_thothan")  ;U+0E10 THAI CHARACTER THO THAN
(define-gtkcode #x0db1 "Thai_thonangmontho") ;U+0E11 THAI CHARACTER THO NANGMONTHO
(define-gtkcode #x0db2 "Thai_thophuthao") ;U+0E12 THAI CHARACTER THO PHUTHAO
(define-gtkcode #x0db3 "Thai_nonen")     ;U+0E13 THAI CHARACTER NO NEN
(define-gtkcode #x0db4 "Thai_dodek")     ;U+0E14 THAI CHARACTER DO DEK
(define-gtkcode #x0db5 "Thai_totao")     ;U+0E15 THAI CHARACTER TO TAO
(define-gtkcode #x0db6 "Thai_thothung") ;U+0E16 THAI CHARACTER THO THUNG
(define-gtkcode #x0db7 "Thai_thothahan") ;U+0E17 THAI CHARACTER THO THAHAN
(define-gtkcode #x0db8 "Thai_thothong") ;U+0E18 THAI CHARACTER THO THONG
(define-gtkcode #x0db9 "Thai_nonu")      ;U+0E19 THAI CHARACTER NO NU
(define-gtkcode #x0dba "Thai_bobaimai") ;U+0E1A THAI CHARACTER BO BAIMAI
(define-gtkcode #x0dbb "Thai_popla")     ;U+0E1B THAI CHARACTER PO PLA
(define-gtkcode #x0dbc "Thai_phophung") ;U+0E1C THAI CHARACTER PHO PHUNG
(define-gtkcode #x0dbd "Thai_fofa")      ;U+0E1D THAI CHARACTER FO FA
(define-gtkcode #x0dbe "Thai_phophan")  ;U+0E1E THAI CHARACTER PHO PHAN
(define-gtkcode #x0dbf "Thai_fofan")     ;U+0E1F THAI CHARACTER FO FAN
(define-gtkcode #x0dc0 "Thai_phosamphao") ;U+0E20 THAI CHARACTER PHO SAMPHAO
(define-gtkcode #x0dc1 "Thai_moma")      ;U+0E21 THAI CHARACTER MO MA
(define-gtkcode #x0dc2 "Thai_yoyak")     ;U+0E22 THAI CHARACTER YO YAK
(define-gtkcode #x0dc3 "Thai_rorua")     ;U+0E23 THAI CHARACTER RO RUA
(define-gtkcode #x0dc4 "Thai_ru")        ;U+0E24 THAI CHARACTER RU
(define-gtkcode #x0dc5 "Thai_loling")    ;U+0E25 THAI CHARACTER LO LING
(define-gtkcode #x0dc6 "Thai_lu")        ;U+0E26 THAI CHARACTER LU
(define-gtkcode #x0dc7 "Thai_wowaen")    ;U+0E27 THAI CHARACTER WO WAEN
(define-gtkcode #x0dc8 "Thai_sosala")    ;U+0E28 THAI CHARACTER SO SALA
(define-gtkcode #x0dc9 "Thai_sorusi")    ;U+0E29 THAI CHARACTER SO RUSI
(define-gtkcode #x0dca "Thai_sosua")     ;U+0E2A THAI CHARACTER SO SUA
(define-gtkcode #x0dcb "Thai_hohip")     ;U+0E2B THAI CHARACTER HO HIP
(define-gtkcode #x0dcc "Thai_lochula")  ;U+0E2C THAI CHARACTER LO CHULA
(define-gtkcode #x0dcd "Thai_oang")      ;U+0E2D THAI CHARACTER O ANG
(define-gtkcode #x0dce "Thai_honokhuk") ;U+0E2E THAI CHARACTER HO NOKHUK
(define-gtkcode #x0dcf "Thai_paiyannoi") ;U+0E2F THAI CHARACTER PAIYANNOI
(define-gtkcode #x0dd0 "Thai_saraa")     ;U+0E30 THAI CHARACTER SARA A
(define-gtkcode #x0dd1 "Thai_maihanakat") ;U+0E31 THAI CHARACTER MAI HAN-AKAT
(define-gtkcode #x0dd2 "Thai_saraaa")    ;U+0E32 THAI CHARACTER SARA AA
(define-gtkcode #x0dd3 "Thai_saraam")    ;U+0E33 THAI CHARACTER SARA AM
(define-gtkcode #x0dd4 "Thai_sarai")     ;U+0E34 THAI CHARACTER SARA I
(define-gtkcode #x0dd5 "Thai_saraii")    ;U+0E35 THAI CHARACTER SARA II
(define-gtkcode #x0dd6 "Thai_saraue")    ;U+0E36 THAI CHARACTER SARA UE
(define-gtkcode #x0dd7 "Thai_sarauee")  ;U+0E37 THAI CHARACTER SARA UEE
(define-gtkcode #x0dd8 "Thai_sarau")     ;U+0E38 THAI CHARACTER SARA U
(define-gtkcode #x0dd9 "Thai_sarauu")    ;U+0E39 THAI CHARACTER SARA UU
(define-gtkcode #x0dda "Thai_phinthu")   ;U+0E3A THAI CHARACTER PHINTHU
(define-gtkcode #x0dde "Thai_maihanakat_maitho")
(define-gtkcode #x0ddf "Thai_baht")   ;U+0E3F THAI CURRENCY SYMBOL BAHT
(define-gtkcode #x0de0 "Thai_sarae")     ;U+0E40 THAI CHARACTER SARA E
(define-gtkcode #x0de1 "Thai_saraae")    ;U+0E41 THAI CHARACTER SARA AE
(define-gtkcode #x0de2 "Thai_sarao")     ;U+0E42 THAI CHARACTER SARA O
(define-gtkcode #x0de3 "Thai_saraaimaimuan") ;U+0E43 THAI CHARACTER SARA AI MAIMUAN
(define-gtkcode #x0de4 "Thai_saraaimaimalai") ;U+0E44 THAI CHARACTER SARA AI MAIMALAI
(define-gtkcode #x0de5 "Thai_lakkhangyao") ;U+0E45 THAI CHARACTER LAKKHANGYAO
(define-gtkcode #x0de6 "Thai_maiyamok") ;U+0E46 THAI CHARACTER MAIYAMOK
(define-gtkcode #x0de7 "Thai_maitaikhu") ;U+0E47 THAI CHARACTER MAITAIKHU
(define-gtkcode #x0de8 "Thai_maiek")     ;U+0E48 THAI CHARACTER MAI EK
(define-gtkcode #x0de9 "Thai_maitho")    ;U+0E49 THAI CHARACTER MAI THO
(define-gtkcode #x0dea "Thai_maitri")    ;U+0E4A THAI CHARACTER MAI TRI
(define-gtkcode #x0deb "Thai_maichattawa") ;U+0E4B THAI CHARACTER MAI CHATTAWA
(define-gtkcode #x0dec "Thai_thanthakhat") ;U+0E4C THAI CHARACTER THANTHAKHAT
(define-gtkcode #x0ded "Thai_nikhahit") ;U+0E4D THAI CHARACTER NIKHAHIT
(define-gtkcode #x0df0 "Thai_leksun")    ;U+0E50 THAI DIGIT ZERO
(define-gtkcode #x0df1 "Thai_leknung")   ;U+0E51 THAI DIGIT ONE
(define-gtkcode #x0df2 "Thai_leksong")   ;U+0E52 THAI DIGIT TWO
(define-gtkcode #x0df3 "Thai_leksam")    ;U+0E53 THAI DIGIT THREE
(define-gtkcode #x0df4 "Thai_leksi")     ;U+0E54 THAI DIGIT FOUR
(define-gtkcode #x0df5 "Thai_lekha")     ;U+0E55 THAI DIGIT FIVE
(define-gtkcode #x0df6 "Thai_lekhok")    ;U+0E56 THAI DIGIT SIX
(define-gtkcode #x0df7 "Thai_lekchet")   ;U+0E57 THAI DIGIT SEVEN
(define-gtkcode #x0df8 "Thai_lekpaet")   ;U+0E58 THAI DIGIT EIGHT
(define-gtkcode #x0df9 "Thai_lekkao")    ;U+0E59 THAI DIGIT NINE
(define-gtkcode #xff31 "Hangul")         ;Hangul start/stop(toggle)
(define-gtkcode #xff32 "Hangul_Start")   ;Hangul start
(define-gtkcode #xff33 "Hangul_End")     ;Hangul end, English start
(define-gtkcode #xff34 "Hangul_Hanja")  ;Start Hangul->Hanja Conversion
(define-gtkcode #xff35 "Hangul_Jamo")    ;Hangul Jamo mode
(define-gtkcode #xff36 "Hangul_Romaja")  ;Hangul Romaja mode
(define-gtkcode #xff37 "Hangul_Codeinput") ;Hangul code input mode
(define-gtkcode #xff38 "Hangul_Jeonja")  ;Jeonja mode
(define-gtkcode #xff39 "Hangul_Banja")   ;Banja mode
(define-gtkcode #xff3a "Hangul_PreHanja") ;Pre Hanja conversion
(define-gtkcode #xff3b "Hangul_PostHanja") ;Post Hanja conversion
(define-gtkcode #xff3c "Hangul_SingleCandidate") ;Single candidate
(define-gtkcode #xff3d "Hangul_MultipleCandidate") ;Multiple candidate
(define-gtkcode #xff3e "Hangul_PreviousCandidate") ;Previous candidate
(define-gtkcode #xff3f "Hangul_Special") ;Special symbols
(define-gtkcode #xff7e "Hangul_switch")  ;Alias for mode_switch
(define-gtkcode #x0ea1 "Hangul_Kiyeog")
(define-gtkcode #x0ea2 "Hangul_SsangKiyeog")
(define-gtkcode #x0ea3 "Hangul_KiyeogSios")
(define-gtkcode #x0ea4 "Hangul_Nieun")
(define-gtkcode #x0ea5 "Hangul_NieunJieuj")
(define-gtkcode #x0ea6 "Hangul_NieunHieuh")
(define-gtkcode #x0ea7 "Hangul_Dikeud")
(define-gtkcode #x0ea8 "Hangul_SsangDikeud")
(define-gtkcode #x0ea9 "Hangul_Rieul")
(define-gtkcode #x0eaa "Hangul_RieulKiyeog")
(define-gtkcode #x0eab "Hangul_RieulMieum")
(define-gtkcode #x0eac "Hangul_RieulPieub")
(define-gtkcode #x0ead "Hangul_RieulSios")
(define-gtkcode #x0eae "Hangul_RieulTieut")
(define-gtkcode #x0eaf "Hangul_RieulPhieuf")
(define-gtkcode #x0eb0 "Hangul_RieulHieuh")
(define-gtkcode #x0eb1 "Hangul_Mieum")
(define-gtkcode #x0eb2 "Hangul_Pieub")
(define-gtkcode #x0eb3 "Hangul_SsangPieub")
(define-gtkcode #x0eb4 "Hangul_PieubSios")
(define-gtkcode #x0eb5 "Hangul_Sios")
(define-gtkcode #x0eb6 "Hangul_SsangSios")
(define-gtkcode #x0eb7 "Hangul_Ieung")
(define-gtkcode #x0eb8 "Hangul_Jieuj")
(define-gtkcode #x0eb9 "Hangul_SsangJieuj")
(define-gtkcode #x0eba "Hangul_Cieuc")
(define-gtkcode #x0ebb "Hangul_Khieuq")
(define-gtkcode #x0ebc "Hangul_Tieut")
(define-gtkcode #x0ebd "Hangul_Phieuf")
(define-gtkcode #x0ebe "Hangul_Hieuh")
(define-gtkcode #x0ebf "Hangul_A")
(define-gtkcode #x0ec0 "Hangul_AE")
(define-gtkcode #x0ec1 "Hangul_YA")
(define-gtkcode #x0ec2 "Hangul_YAE")
(define-gtkcode #x0ec3 "Hangul_EO")
(define-gtkcode #x0ec4 "Hangul_E")
(define-gtkcode #x0ec5 "Hangul_YEO")
(define-gtkcode #x0ec6 "Hangul_YE")
(define-gtkcode #x0ec7 "Hangul_O")
(define-gtkcode #x0ec8 "Hangul_WA")
(define-gtkcode #x0ec9 "Hangul_WAE")
(define-gtkcode #x0eca "Hangul_OE")
(define-gtkcode #x0ecb "Hangul_YO")
(define-gtkcode #x0ecc "Hangul_U")
(define-gtkcode #x0ecd "Hangul_WEO")
(define-gtkcode #x0ece "Hangul_WE")
(define-gtkcode #x0ecf "Hangul_WI")
(define-gtkcode #x0ed0 "Hangul_YU")
(define-gtkcode #x0ed1 "Hangul_EU")
(define-gtkcode #x0ed2 "Hangul_YI")
(define-gtkcode #x0ed3 "Hangul_I")
(define-gtkcode #x0ed4 "Hangul_J_Kiyeog")
(define-gtkcode #x0ed5 "Hangul_J_SsangKiyeog")
(define-gtkcode #x0ed6 "Hangul_J_KiyeogSios")
(define-gtkcode #x0ed7 "Hangul_J_Nieun")
(define-gtkcode #x0ed8 "Hangul_J_NieunJieuj")
(define-gtkcode #x0ed9 "Hangul_J_NieunHieuh")
(define-gtkcode #x0eda "Hangul_J_Dikeud")
(define-gtkcode #x0edb "Hangul_J_Rieul")
(define-gtkcode #x0edc "Hangul_J_RieulKiyeog")
(define-gtkcode #x0edd "Hangul_J_RieulMieum")
(define-gtkcode #x0ede "Hangul_J_RieulPieub")
(define-gtkcode #x0edf "Hangul_J_RieulSios")
(define-gtkcode #x0ee0 "Hangul_J_RieulTieut")
(define-gtkcode #x0ee1 "Hangul_J_RieulPhieuf")
(define-gtkcode #x0ee2 "Hangul_J_RieulHieuh")
(define-gtkcode #x0ee3 "Hangul_J_Mieum")
(define-gtkcode #x0ee4 "Hangul_J_Pieub")
(define-gtkcode #x0ee5 "Hangul_J_PieubSios")
(define-gtkcode #x0ee6 "Hangul_J_Sios")
(define-gtkcode #x0ee7 "Hangul_J_SsangSios")
(define-gtkcode #x0ee8 "Hangul_J_Ieung")
(define-gtkcode #x0ee9 "Hangul_J_Jieuj")
(define-gtkcode #x0eea "Hangul_J_Cieuc")
(define-gtkcode #x0eeb "Hangul_J_Khieuq")
(define-gtkcode #x0eec "Hangul_J_Tieut")
(define-gtkcode #x0eed "Hangul_J_Phieuf")
(define-gtkcode #x0eee "Hangul_J_Hieuh")
(define-gtkcode #x0eef "Hangul_RieulYeorinHieuh")
(define-gtkcode #x0ef0 "Hangul_SunkyeongeumMieum")
(define-gtkcode #x0ef1 "Hangul_SunkyeongeumPieub")
(define-gtkcode #x0ef2 "Hangul_PanSios")
(define-gtkcode #x0ef3 "Hangul_KkogjiDalrinIeung")
(define-gtkcode #x0ef4 "Hangul_SunkyeongeumPhieuf")
(define-gtkcode #x0ef5 "Hangul_YeorinHieuh")
(define-gtkcode #x0ef6 "Hangul_AraeA")
(define-gtkcode #x0ef7 "Hangul_AraeAE")
(define-gtkcode #x0ef8 "Hangul_J_PanSios")
(define-gtkcode #x0ef9 "Hangul_J_KkogjiDalrinIeung")
(define-gtkcode #x0efa "Hangul_J_YeorinHieuh")
(define-gtkcode #x0eff "Korean_Won")     ;(U+20A9 WON SIGN)

(define-gtkcode #x20ac "EuroSign")       ;U+20AC EURO SIGN

(defun modifier-p (gtkcode)
  "check if the gtkcode is a modifier (shift, etc)"
  ;; TODO: these should probably be mapped individually...
  (and (>= gtkcode #xffe1)
       (<= gtkcode #xffee)))
