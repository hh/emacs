;;; tld.el --- TLD lookup tool.
;; Copyright 2000 by Dave Pearson <davep@davep.org>
;; $Revision: 1.4 $

;; tld.el is free software distributed under the terms of the GNU
;; General Public Licence, version 2. For details see the file COPYING.

;;; Commentary:
;;
;; tld.el provides a command for looking up TLDs, either by searching for a
;; specific TLD or by searching country names.
;;
;; One command is provided: `tld'.
;;
;; The latest tld.el is always available from:
;;
;;   <URL:http://www.davep.org/emacs/tld.el>
;;
;; Note that, to some degree, this code duplicates the functionality
;; provided by `what-domain' (a command that is part of emacs). tld.el
;; differs slightly in that it allows for both TLD and country name
;; searches. Also, compared to emacs 20.7, the list of TLDs is more complete
;; (yes, I know, I should submit a patch to the emacs maintainers, I will at
;; some point).

;;; INSTALLATION:
;;
;; o Drop tld.el somwehere into your `load-path'. Try your site-lisp
;;   directory for example (you might also want to byte-compile the file).
;;
;; o Add the following autoload statement to your ~/.emacs file:
;;
;;   (autoload 'tld "tld" "Perform a TLD lookup" t)

;;; Code:

;; Things we need:

(eval-when-compile
  (require 'cl))

;; Constants.

(defconst tld-list
    '(("AC"     . "Ascension Island")
      ("AD"     . "Andorra")
      ("AE"     . "United Arab Emirates")
      ("AF"     . "Afghanistan")
      ("AG"     . "Antigua and Barbuda")
      ("AI"     . "Anguilla")
      ("AL"     . "Albania")
      ("AM"     . "Armenia")
      ("AN"     . "Netherlands Antilles")
      ("AO"     . "Angola")
      ("AQ"     . "Antartica")
      ("AR"     . "Argentina")
      ("ARPA"   . "Old style Arpanet obsolete")
      ("AS"     . "American Samoa")
      ("AT"     . "Austria")
      ("AU"     . "Australia")
      ("AW"     . "Aruba")
      ("AZ"     . "Azerbaijan")
      ("BA"     . "Bosnia and Herzegovina")
      ("BB"     . "Barbados")
      ("BD"     . "Bangladesh")
      ("BE"     . "Belgium")
      ("BF"     . "Burkina Faso")
      ("BG"     . "Bulgaria")
      ("BH"     . "Bahrain")
      ("BI"     . "Burundi")
      ("BITNET" . "Pseudo-domain for EARN/BITNET gateway")
      ("BJ"     . "Benin")
      ("BM"     . "Bermuda")
      ("BN"     . "Brunei Darussalam")
      ("BO"     . "Bolivia")
      ("BR"     . "Brazil")
      ("BS"     . "Bahamas")
      ("BT"     . "Bhutan")
      ("BV"     . "Bouvet Island")
      ("BW"     . "Botswana")
      ("BY"     . "Belarus")
      ("BZ"     . "Belize")
      ("CA"     . "Canada")
      ("CC"     . "Cocos (Keeling) Islands")
      ("CD"     . "Congo, Democratic People's Republic")
      ("CF"     . "Central African Republic")
      ("CG"     . "Congo, Republic of")
      ("CH"     . "Switzerland")
      ("CI"     . "Cote d'Ivoire")
      ("CK"     . "Cook Islands")
      ("CL"     . "Chile")
      ("CM"     . "Cameroon")
      ("CN"     . "China")
      ("CO"     . "Colombia")
      ("COM"    . "Commercial")
      ("CR"     . "Costa Rica")
      ("CU"     . "Cuba")
      ("CV"     . "Cap Verde")
      ("CX"     . "Christmas Island")
      ("CY"     . "Cyprus")
      ("CZ"     . "Czech Republic")
      ("DE"     . "Germany")
      ("DJ"     . "Djibouti")
      ("DK"     . "Denmark")
      ("DM"     . "Dominica")
      ("DO"     . "Dominican Republic")
      ("DZ"     . "Algeria")
      ("EC"     . "Ecuador")
      ("EDU"    . "Educational: US only (universities)")
      ("EE"     . "Estonia")
      ("EG"     . "Egypt")
      ("EH"     . "Western Sahara")
      ("ER"     . "Eritrea")
      ("ES"     . "Spain")
      ("ET"     . "Ethiopia")
      ("FI"     . "Finland")
      ("FJ"     . "Fiji")
      ("FK"     . "Falkland Islands (Malvina)")
      ("FM"     . "Micronesia, Federal State of")
      ("FO"     . "Faroe Islands")
      ("FR"     . "France")
      ("GA"     . "Gabon")
      ("GB"     . "Great Britain (UK)")
      ("GD"     . "Grenada")
      ("GE"     . "Georgia")
      ("GF"     . "French Guiana")
      ("GG"     . "Guernsey")
      ("GH"     . "Ghana")
      ("GI"     . "Gibraltar")
      ("GL"     . "Greenland")
      ("GM"     . "Gambia")
      ("GN"     . "Guinea")
      ("GOV"    . "US Government")
      ("GP"     . "Guadeloupe")
      ("GQ"     . "Equatorial Guinea")
      ("GR"     . "Greece")
      ("GS"     . "South Georgia and the South Sandwich Islands")
      ("GT"     . "Guatemala")
      ("GU"     . "Guam")
      ("GW"     . "Guinea-Bissau")
      ("GY"     . "Guyana")
      ("HK"     . "Hong Kong")
      ("HM"     . "Heard and McDonald Islands")
      ("HN"     . "Honduras")
      ("HR"     . "Croatia/Hrvatska")
      ("HT"     . "Haiti")
      ("HU"     . "Hungary")
      ("ID"     . "Indonesia")
      ("IE"     . "Ireland")
      ("IL"     . "Israel")
      ("IM"     . "Isle of Man")
      ("IN"     . "India")
      ("INT"    . "International field: Nato")
      ("IO"     . "British Indian Ocean Territory")
      ("IQ"     . "Iraq")
      ("IR"     . "Iran (Islamic Republic of)")
      ("IS"     . "Iceland")
      ("IT"     . "Italy")
      ("JE"     . "Jersey")
      ("JM"     . "Jamaica")
      ("JO"     . "Jordan")
      ("JP"     . "Japan")
      ("KE"     . "Kenya")
      ("KG"     . "Kyrgyzstan")
      ("KH"     . "Cambodia")
      ("KI"     . "Kiribati")
      ("KM"     . "Comoros")
      ("KN"     . "Saint Kitts and Nevis")
      ("KP"     . "Korea, Democratic People's Republic")
      ("KR"     . "Korea, Republic of")
      ("KW"     . "Kuwait")
      ("KY"     . "Cayman Islands")
      ("KZ"     . "Kazakhstan")
      ("LA"     . "Lao People's Democratic Republic")
      ("LB"     . "Lebanon")
      ("LC"     . "Saint Lucia")
      ("LI"     . "Liechtenstein")
      ("LK"     . "Sri Lanka")
      ("LR"     . "Liberia")
      ("LS"     . "Lesotho")
      ("LT"     . "Lithuania")
      ("LU"     . "Luxembourg")
      ("LV"     . "Latvia")
      ("LY"     . "Libyan Arab Jamahiriya")
      ("MA"     . "Morocco")
      ("MC"     . "Monaco")
      ("MD"     . "Moldova, Republic of")
      ("MG"     . "Madagascar")
      ("MH"     . "Marshall Islands")
      ("MIL"    . "Military: US only")
      ("MK"     . "Macedonia, Former Yugoslav Republic")
      ("ML"     . "Mali")
      ("MM"     . "Myanmar")
      ("MN"     . "Mongolia")
      ("MO"     . "Macau")
      ("MP"     . "Northern Mariana Islands")
      ("MQ"     . "Martinique")
      ("MR"     . "Mauritania")
      ("MS"     . "Montserrat")
      ("MT"     . "Malta")
      ("MU"     . "Mauritius")
      ("MV"     . "Maldives")
      ("MW"     . "Malawi")
      ("MX"     . "Mexico")
      ("MY"     . "Malaysia")
      ("MZ"     . "Mozambique")
      ("NA"     . "Namibia")
      ("NATO"   . "Nato field: obsolete")
      ("NC"     . "New Caledonia")
      ("NE"     . "Niger")
      ("NET"    . "Network")
      ("NF"     . "Norfolk Island")
      ("NG"     . "Nigeria")
      ("NI"     . "Nicaragua")
      ("NL"     . "Netherlands")
      ("NO"     . "Norway")
      ("NP"     . "Nepal")
      ("NR"     . "Nauru")
      ("NT"     . "Neutral Zone")
      ("NU"     . "Niue")
      ("NZ"     . "New Zealand")
      ("OM"     . "Oman")
      ("ORG"    . "Non-Profit Organization")
      ("PA"     . "Panama")
      ("PE"     . "Peru")
      ("PF"     . "French Polynesia")
      ("PG"     . "Papua New Guinea")
      ("PH"     . "Philippines")
      ("PK"     . "Pakistan")
      ("PL"     . "Poland")
      ("PM"     . "St. Pierre and Miquelon")
      ("PN"     . "Pitcairn Island")
      ("PR"     . "Puerto Rico")
      ("PS"     . "Palestinian Territories")
      ("PT"     . "Portugal")
      ("PW"     . "Palau")
      ("PY"     . "Paraguay")
      ("QA"     . "Qatar")
      ("RE"     . "Reunion Island")
      ("RO"     . "Romania")
      ("RU"     . "Russian Federation")
      ("RW"     . "Rwanda")
      ("SA"     . "Saudi Arabia")
      ("SB"     . "Solomon Islands")
      ("SC"     . "Seychelles")
      ("SD"     . "Sudan")
      ("SE"     . "Sweden")
      ("SG"     . "Singapore")
      ("SH"     . "St. Helena")
      ("SI"     . "Slovenia")
      ("SJ"     . "Svalbard and Jan Mayen Islands")
      ("SK"     . "Slovak Republic")
      ("SL"     . "Sierra Leone")
      ("SM"     . "San Marino")
      ("SN"     . "Senegal")
      ("SO"     . "Somalia")
      ("SR"     . "Suriname")
      ("ST"     . "Sao Tome and Principe")
      ("SU"     . "Soviet Union")
      ("SV"     . "El Salvador")
      ("SY"     . "Syrian Arab Republic")
      ("SZ"     . "Swaziland")
      ("TC"     . "Turks and Ciacos Islands")
      ("TD"     . "Chad")
      ("TF"     . "French Southern Territories")
      ("TG"     . "Togo")
      ("TH"     . "Thailand")
      ("TJ"     . "Tajikistan")
      ("TK"	. "Tokelau")
      ("TM"	. "Turkmenistan")
      ("TN"	. "Tunisia")
      ("TO"	. "Tonga")
      ("TP"	. "East Timor")
      ("TR"	. "Turkey")
      ("TT"	. "Trinidad and Tobago")
      ("TV"	. "Tuvalu")
      ("TW"	. "Taiwan")
      ("TZ"	. "Tanzania")
      ("UA"	. "Ukraine")
      ("UG"	. "Uganda")
      ("UK"	. "United Kingdom")
      ("UM"	. "US Minor Outlying Islands")
      ("US"	. "United States")
      ("UUCP"	. "Pseudo-domain for UUCP gateway")
      ("UY"	. "Uruguay")
      ("UZ"	. "Uzbekistan")
      ("VA"	. "Holy See (City Vatican State)")
      ("VC"	. "Saint Vincent and the Grenadines")
      ("VE"	. "Venezuela")
      ("VG"	. "Virgin Islands (British)")
      ("VI"	. "Virgin Islands (USA)")
      ("VN"	. "Vietnam")
      ("VU"	. "Vanuatu")
      ("WF"	. "Wallis and Futuna Islands")
      ("WS"	. "Western Samoa")
      ("YE"	. "Yemen")
      ("YT"	. "Mayotte")
      ("YU"	. "Yugoslavia")
      ("ZA"	. "South Africa")
      ("ZM"	. "Zambia")
      ("ZR"	. "Zaire")
      ("ZW"	. "Zimbabwe"))
  "Association list of TLDs.")

;; Main code.

(defsubst tld-tld (tld)
  "Return the TLD portion of a TLD pair."
  (car tld))

(defsubst tld-name (tld)
  "Return the name portion of a TLD pair."
  (cdr tld))

(defun tld-find-tld (tld)
  "Lookup a TLD.

If found a (TLD . NAME) pair is returned."
  (assoc (upcase tld) tld-list))

(defun tld-find-name (name)
  "Lookup a name.

Returns a list of hits."
  (let ((case-fold-search t))
    (loop for tld in tld-list
          when (string-match name (tld-name tld))
          collect tld)))

;;;###autoload
(defun tld (search)
  "Search the TLD list."
  (interactive "sSearch: ")
  (let* ((tld-lookup (string= (substring search 0 1) "."))
         (result     (if tld-lookup (tld-find-tld (substring search 1)) (tld-find-name search))))
    (if result
        (flet ((message-tld (tld)
                 (message "%s is %s" (tld-tld tld) (tld-name tld))))
          (if tld-lookup
              (message-tld result)
            (if (= (length result) 1)
                (message-tld (car result))
              (with-output-to-temp-buffer "*tld*"
                (princ "TLD    Name\n====== ========================================\n\n")
                (loop for tld in result
                      do (princ (format "%-6s %s\n" (tld-tld tld) (tld-name tld))))))))
      ;; If nothing was found and it wasn't a tld-lookup but it looks like
      ;; it might be a TLD re-submit it with a leading dot.
      (if (and (not tld-lookup) (< (length search) 7))
          (tld (concat "." search))
        (error "No TLD match found")))))

(provide 'tld)

;;; tld.el ends here
