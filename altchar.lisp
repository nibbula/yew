;;
;; altchar.lisp - Type with alternate character sets.
;;

(defpackage :altchar
  (:documentation "Type with alternate character sets.")
  (:use :cl :char-util :keymap :options :rl :pick-list :inator)
  (:export
   #:altchar-mode
   #:exit-altchar-mode
   #:altchar-insert-command
   #:pick-altchar
   #:pick-altchar-command
   #:bind-keys
   ))
(in-package :altchar)

(defstruct alphabet
  "Map of the latin/ASCII alphabet."
  name
  upper-map		     ; Mapping of uppercase latin characters.
  lower-map		     ; Mapping of lowercase latin characters.
  digits		     ; Latin digits 0-9
  punctuation		     ; Punctuation in the order it appears in ASCII
  keymap)		     ; For faster lookup.

(defmethod print-object ((object alphabet) stream)
  "Print an ALPHABET structure to STREAM. This prints it's name nicely, is so
we can use pick-list effectively."
  (with-slots (name) object
    (if *print-escape* ;; 
	(print-unreadable-object (object stream :type t)
	  (format stream "~a" name))
	(write-string name stream))))

(defparameter *upper* "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defparameter *lower* "abcdefghijklmnopqrstuvwxyz")
(defparameter *digits* "0123456789")
(defparameter *punctuation* "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")

(defparameter *alphabet-data*
  '(("𝐁𝐨𝐥𝐝"
     "𝐀𝐁𝐂𝐃𝐄𝐅𝐆𝐇𝐈𝐉𝐊𝐋𝐌𝐍𝐎𝐏𝐐𝐑𝐒𝐓𝐔𝐕𝐖𝐗𝐘𝐙"
     "𝐚𝐛𝐜𝐝𝐞𝐟𝐠𝐡𝐢𝐣𝐤𝐥𝐦𝐧𝐨𝐩𝐪𝐫𝐬𝐭𝐮𝐯𝐰𝐱𝐲𝐳"
     "𝟎𝟏𝟐𝟑𝟒𝟓𝟔𝟕𝟖𝟗")
    ("𝐼𝑡𝑎𝑙𝑖𝑐"
     "𝐴𝐵𝐶𝐷𝐸𝐹𝐺𝐻𝐼𝐽𝐾𝐿𝑀𝑁𝑂𝑃𝑄𝑅𝑆𝑇𝑈𝑉𝑊𝑋𝑌𝑍"
     "𝑎𝑏𝑐𝑑𝑒𝑓𝑔𝑖𝑗𝑘𝑙𝑚𝑛𝑜𝑝𝑞𝑟𝑠𝑡𝑢𝑣𝑤𝑥𝑦𝑧")
    ("𝑩𝒐𝒍𝒅 𝑰𝒕𝒂𝒍𝒊𝒄"
     "𝑨𝑩𝑪𝑫𝑬𝑭𝑮𝑯𝑰𝑱𝑲𝑳𝑴𝑵𝑶𝑷𝑸𝑹𝑺𝑻𝑼𝑽𝑾𝑿𝒀𝒁"
     "𝒂𝒃𝒄𝒅𝒆𝒇𝒈𝒉𝒊𝒋𝒌𝒍𝒎𝒏𝒐𝒑𝒒𝒓𝒔𝒕𝒖𝒗𝒘𝒙𝒚𝒛")
    ("𝒮𝒸𝓇𝒾𝓅𝓉"
     "𝒜𝒞𝒟𝒢𝒥𝒦𝒩𝒪𝒫𝒬𝒮𝒯𝒰𝒱𝒲𝒳𝒴𝒵"
     "𝒶𝒷𝒸𝒹𝒻𝒽𝒾𝒿𝓀𝓁𝓂𝓃𝓅𝓆𝓇𝓈𝓉𝓊𝓋𝓌𝓍𝓎𝓏")
    ("𝓑𝓸𝓵𝓭 𝓢𝓬𝓻𝓲𝓹𝓽"
     "𝓐𝓑𝓒𝓓𝓔𝓕𝓖𝓗𝓘𝓙𝓚𝓛𝓜𝓝𝓞𝓟𝓠𝓡𝓢𝓣𝓤𝓥𝓦𝓧𝓨𝓩"
     "𝓪𝓫𝓬𝓭𝓮𝓯𝓰𝓱𝓲𝓳𝓴𝓵𝓶𝓷𝓸𝓹𝓺𝓻𝓼𝓽𝓾𝓿𝔀𝔁𝔂𝔃")
    ("𝔉𝔯𝔞𝔠𝔱𝔲𝔯"
     "𝔄𝔅𝔇𝔈𝔉𝔊𝔍𝔎𝔏𝔐𝔑𝔒𝔓𝔔𝔖𝔗𝔘𝔙𝔚𝔛𝔜"
     "𝔞𝔟𝔠𝔡𝔢𝔣𝔤𝔥𝔦𝔧𝔨𝔩𝔪𝔫𝔬𝔭𝔮𝔯𝔰𝔱𝔲𝔳𝔴𝔵𝔶𝔷")
    ("𝔻𝕠𝕦𝕓𝕝𝕖-𝕊𝕥𝕣𝕦𝕔𝕜"
     "𝔸𝔹ℂ𝔻𝔼𝔽𝔾ℍ𝕀𝕁𝕂𝕃𝕄ℕ𝕆ℙℚℝ𝕊𝕋𝕌𝕍𝕎𝕏𝕐ℤ"
     "𝕒𝕓𝕔𝕕𝕖𝕗𝕘𝕙𝕚𝕛𝕜𝕝𝕞𝕟𝕠𝕡𝕢𝕣𝕤𝕥𝕦𝕧𝕨𝕩𝕪𝕫"
     "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡"
     "❕\"#$%&'⦅⦆*+,-./:;<=>?@⟦\\⟧^_`⦃⫾⦄~"
     )
    ("𝕭𝖔𝖑𝖉 𝕱𝖗𝖆𝖈𝖙𝖚𝖗"
     "𝕬𝕭𝕮𝕯𝕰𝕱𝕲𝕳𝕴𝕵𝕶𝕷𝕸𝕹𝕺𝕻𝕼𝕽𝕾𝕿𝖀𝖁𝖂𝖃𝖄𝖅"
     "𝖆𝖇𝖈𝖉𝖊𝖋𝖌𝖍𝖎𝖏𝖐𝖑𝖒𝖓𝖔𝖕𝖖𝖗𝖘𝖙𝖚𝖛𝖜𝖝𝖞𝖟")
    ("𝖲𝖺𝗇𝗌-𝖲𝖾𝗋𝗂𝖿"
     "𝖠𝖡𝖢𝖣𝖤𝖥𝖦𝖧𝖨𝖩𝖪𝖫𝖬𝖭𝖮𝖯𝖰𝖱𝖲𝖳𝖴𝖵𝖶𝖷𝖸𝖹"
     "𝖺𝖻𝖼𝖽𝖾𝖿𝗀𝗁𝗂𝗃𝗄𝗅𝗆𝗇𝗈𝗉𝗊𝗋𝗌𝗍𝗎𝗏𝗐𝗑𝗒𝗓"
     "𝟢𝟣𝟤𝟥𝟦𝟧𝟨𝟩𝟪𝟫")
    ("𝗦𝗮𝗻𝘀-𝗦𝗲𝗿𝗶𝗳 𝗕𝗼𝗹𝗱"
     "𝗔𝗕𝗖𝗗𝗘𝗙𝗚𝗛𝗜𝗝𝗞𝗟𝗠𝗡𝗢𝗣𝗤𝗥𝗦𝗧𝗨𝗩𝗪𝗫𝗬𝗭"
     "𝗮𝗯𝗰𝗱𝗲𝗳𝗴𝗵𝗶𝗷𝗸𝗹𝗺𝗻𝗼𝗽𝗾𝗿𝘀𝘁𝘂𝘃𝘄𝘅𝘆𝘇"
     "𝟬𝟭𝟮𝟯𝟰𝟱𝟲𝟳𝟴𝟵")
    ("𝘚𝘢𝘯𝘴-𝘚𝘦𝘳𝘪𝘧 𝘐𝘵𝘢𝘭𝘪𝘤"
     "𝘈𝘉𝘊𝘋𝘌𝘍𝘎𝘏𝘐𝘑𝘒𝘓𝘔𝘕𝘖𝘗𝘘𝘙𝘚𝘛𝘜𝘝𝘞𝘟𝘠𝘡"
     "𝘢𝘣𝘤𝘥𝘦𝘧𝘨𝘩𝘪𝘫𝘬𝘭𝘮𝘯𝘰𝘱𝘲𝘳𝘴𝘵𝘶𝘷𝘸𝘹𝘺𝘻")
    ("𝙎𝙖𝙣𝙨-𝙎𝙚𝙧𝙞𝙛 𝘽𝙤𝙡𝙙 𝙄𝙩𝙖𝙡𝙞𝙘"
     "𝘼𝘽𝘾𝘿𝙀𝙁𝙂𝙃𝙄𝙅𝙆𝙇𝙈𝙉𝙊𝙋𝙌𝙍𝙎𝙏𝙐𝙑𝙒𝙓𝙔𝙕"
     "𝙖𝙗𝙘𝙙𝙚𝙛𝙜𝙝𝙞𝙟𝙠𝙡𝙢𝙣𝙤𝙥𝙦𝙧𝙨𝙩𝙪𝙫𝙬𝙭𝙮𝙯")
    ("𝙼𝚘𝚗𝚘𝚜𝚙𝚊𝚌𝚎"
     "𝙰𝙱𝙲𝙳𝙴𝙵𝙶𝙷𝙸𝙹𝙺𝙻𝙼𝙽𝙾𝙿𝚀𝚁𝚂𝚃𝚄𝚅𝚆𝚇𝚈𝚉"
     "𝚊𝚋𝚌𝚍𝚎𝚏𝚐𝚑𝚒𝚓𝚔𝚕𝚖𝚗𝚘𝚙𝚚𝚛𝚜𝚝𝚞𝚟𝚠𝚡𝚢𝚣"
     "𝟶𝟷𝟸𝟹𝟺𝟻𝟼𝟽𝟾𝟿")
    ("Ｆｕｌｌｗｉｄｔｈ"
     "ＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ"
     "ａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚ"
    "０１２３４５６７８９"
     "！＂＃＄％＆＇（）＊＋，－．／：；＜＝＞？＠［＼］＾＿｀｛｜｝～")
    ("ᗯᕮIᖇᗪ"
     "ᗩᗷᑕᗪᕮᖴGᕼIᒍKᒪᗰᑎOᑭᑫᖇᔕTᑌᐯᗯ᙭Yᘔ"
     "ᗩᗷᑕᗪᕮᖴGᕼIᒍKᒪᗰᑎOᑭᑫᖇᔕTᑌᐯᗯ᙭Yᘔ")
    ("J₳₦₭Ɏ"
    "₳฿₵ĐɆ₣₲ⱧłJ₭Ⱡ₥₦Ø₱QⱤ₴₮ɄV₩ӾɎⱫ"
    "₳฿₵ĐɆ₣₲ⱧłJ₭Ⱡ₥₦Ø₱QⱤ₴₮ɄV₩ӾɎⱫ")
    ("Ⓒⓘⓡⓒⓛⓔⓓ"
     "ⒶⒷⒸⒹⒺⒻⒼⒽⒾⒿⓀⓁⓂⓃⓄⓅⓆⓇⓈⓉⓊⓋⓌⓍⓎⓏ"
     "ⓐⓑⓒⓓⓔⓕⓖⓗⓘⓙⓚⓛⓜⓝⓞⓟⓠⓡⓢⓣⓤⓥⓦⓧⓨⓩ"
     "⓪①②③④⑤⑥⑦⑧⑨")
    ("🅂🅀🅄🄰🅁🄴🄳"
     "🄰🄱🄲🄳🄴🄵🄶🄷🄸🄹🄺🄻🄼🄽🄾🄿🅀🅁🅂🅃🅄🅅🅆🅇🅈🅉"
     "🄰🄱🄲🄳🄴🄵🄶🄷🄸🄹🄺🄻🄼🄽🄾🄿🅀🅁🅂🅃🅄🅅🅆🅇🅈🅉")
    ("🅽🅴🅶🅰🆃🅸🆅🅴 🆂🆀🆄🅰🆁🅴🅳"
     "🅰🅱🅲🅳🅴🅵🅶🅷🅸🅹🅺🅻🅼🅽🅾🅿🆀🆁🆂🆃🆄🆅🆆🆇🆈🆉"
     "🅰🅱🅲🅳🅴🅵🅶🅷🅸🅹🅺🅻🅼🅽🅾🅿🆀🆁🆂🆃🆄🆅🆆🆇🆈🆉")
    ("⒫⒜⒭⒠⒯⒣⒠⒮⒤⒵⒠⒟"
     "⒜⒝⒞⒟⒠⒡⒢⒣⒤⒥⒦⒧⒨⒩⒪⒫⒬⒭⒮⒯⒰⒱⒲⒳⒴⒵"
     "⒜⒝⒞⒟⒠⒡⒢⒣⒤⒥⒦⒧⒨⒩⒪⒫⒬⒭⒮⒯⒰⒱⒲⒳⒴⒵"
     "0⑴⑵⑶⑷⑸⑹⑺⑻⑼")
))

(defun bind-letters (keymap letters base-letters)
  (loop
     :for from :across base-letters
     :for to :across letters
     :do
     (define-key keymap from 'altchar-insert-command)))

(defparameter *alphabets*
  (loop :for a :in *alphabet-data*
     :collect
     (let ((set (make-alphabet :name        (first a)
			       :upper-map   (second a)
			       :lower-map   (third a)
			       :digits      (fourth a)
			       :punctuation (fifth a)))
	   (keymap (make-instance 'keymap:keymap)))
       (when (alphabet-upper-map set)
	 (bind-letters keymap (alphabet-upper-map set) *upper*))
       (when (alphabet-lower-map set)
	 (bind-letters keymap (alphabet-lower-map set) *lower*))
       (when (alphabet-digits set)
	 (bind-letters keymap (alphabet-digits set) *digits*))
       (when (alphabet-punctuation set)
	 (bind-letters keymap (alphabet-punctuation set) *punctuation*))
       (setf (alphabet-keymap set) keymap)
       set))
  "List of alphabets.")

(defoption line-editor saved-default-binding option :value nil
  :documentation
  "The default key binding before we turned on altchar-mode, so we can
restore it.")

(defoption line-editor character-set option :value nil
  :documentation "The alternate character set.")

(defoption line-editor altchar-mode option :value nil
  :documentation "True if altchar-mode is active.")

(defun find-char (c alphabet)
  (let (pos)
    (cond
      ((setf pos (position c *upper*))
       (aref (alphabet-upper-map alphabet) pos))
      ((setf pos (position c *lower*))
       (aref (alphabet-lower-map alphabet) pos))
      ((setf pos (position c *digits*))
       (aref (alphabet-digits alphabet) pos))
      ((setf pos (position c *punctuation*))
       (aref (alphabet-punctuation alphabet) pos))
      (t c))))

(defmulti-method altchar-insert-command ((e line-editor))
  (let ((set (line-editor-character-set e)))
    (when set
      ;; (let ((char (key-binding (rl::last-event e) (alphabet-keymap set))))
      (let ((char (find-char (rl::last-event e) set)))
	;; (message e "~s" char)
	(if char
	    (self-insert e :char char)
	    ;;(process-event e (rl::last-event e) (inator-keymap e))
	    (self-insert e)
	    )))))

(defun altchar-mode (e &optional (state t state-provided-p))
  "Toggle or set altchar-mode."
  (with-slots ((local-keymap rl::local-keymap)) e
    (if (if state-provided-p (not state) (line-editor-altchar-mode e))
	(progn
	  ;;(setf (keymap-default-binding local-keymap) nil)
	  (if (and (line-editor-character-set e)
		   (alphabet-p (line-editor-character-set e)))
	      (remove-keymap (alphabet-keymap (line-editor-character-set e))
			     ;; local-keymap
			     (inator-keymap e)
			     )
	      (message e "You need to pick a character set for this to work."))
	  (setf (line-editor-altchar-mode e) nil))
	(progn
	  ;;(setf (keymap-default-binding local-keymap) 'altchar-insert-command
	  (when (and (line-editor-character-set e)
		     (alphabet-p (line-editor-character-set e)))
	    (push-keymap (alphabet-keymap (line-editor-character-set e))
			 ;; local-keymap
			 (inator-keymap e)
			 ))
	  (setf (line-editor-altchar-mode e) t))))
  (message e "Altchar mode ~:[off~;on~]." (line-editor-altchar-mode e)))

(defun pick-altchar ()
  (pick-list (loop :for a :in *alphabets* :collect a)))

(defun pick-altchar-command (e)
  (setf (line-editor-character-set e) (pick-altchar)))

(defun bind-keys ()
  (keymap:set-key #\a 'altchar-mode rl::*ctlx-keymap*)
  (keymap:set-key (ctrl #\a) 'pick-altchar-command rl::*ctlx-keymap*)
  (values))

;; End
