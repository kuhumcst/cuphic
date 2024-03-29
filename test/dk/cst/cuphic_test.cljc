(ns dk.cst.cuphic-test
  (:require [clojure.test :refer [deftest are is testing]]
            [dk.cst.cuphic :as cup]))

(def document
  [:TEI
   {:xmlns "http://www.tei-c.org/ns/1.0", :xmlns/t "http://www.tei-c.org/ns/1.0"}
   [:teiHeader
    {}
    [:fileDesc
     #:xml{:lang "da"}
     [:titleStmt
      {}
      [:title {} "testfil"]
      [:author {:ref "#xx"} "AFSENDER"]
      [:editor {:ref "#xx", :role "commenting"}]
      [:funder {} "Carlsbergfondet, Forskningsinfrastruktur, Infrastrukturalisme, 2018-2021"]
      [:respStmt {} [:resp {} "Transskription"] [:persName {:ref "xx"} "Mads Nielsen"]]
      [:respStmt {} [:resp {} "Digitalisering"] [:orgName {} "Digitaliseringsafdelingen, Det Kongelige Bibliotek"]]]
     [:publicationStmt
      {}
      [:publisher {} "Henrik Jørgensen, Aarhus Universitet"]
      [:pubPlace {} "CLARIN-DK"]
      [:date {:when "2020"}]]
     [:sourceDesc
      {}
      [:msDesc
       {}
       [:msIdentifier
        {}
        [:settlement {:ref "#København"} "København"]
        [:repository {:ref "xx"} "Det Kongelige Bibliotek"]
        [:idno {}]]
       [:physDesc
        {}
        [:objectDesc {:form "letter"} [:supportDesc {} [:support {} "Gennemslag"] [:extent {} [:note {} "4"]]]]
        [:handDesc {} [:p {} "Maskinskrevet"]]]]]]
    [:profileDesc
     {}
     [:correspDesc
      {}
      [:correspAction
       {:type "sent"}
       [:persName {:ref "#xx"} "AFSENDER"]
       [:placeName {:ref "#xx"} "STED"]
       [:date {} "1942-06-30"]]
      [:correspAction {:type "received"} [:persName {:ref "#xx"} "MODTAGER"] [:placeName {:ref "#xx"}]]]
     [:langUsage {} [:language {:ident "da"} "da"]]]
    [:revisionDesc
     {}
     [:listChange {} [:change {:when "2020", :who "#dortehaltruphansen"} "Automatisk generering af TEI-format"]]]]
   [:facsimile {} [:graphic {:mimeType "image/pdf", :url "Kps24.628", :xml/id "Kps24.628"}]]
   [:facsimile {} [:graphic {:mimeType "image/pdf", :url " Kps24.629", :xml/id " Kps24.629"}]]
   [:facsimile {} [:graphic {:mimeType "image/pdf", :url " Kps24.630", :xml/id " Kps24.630"}]]
   [:facsimile {} [:graphic {:mimeType "image/pdf", :url " Kps24.6231", :xml/id " Kps24.6231"}]]
   [:text
    {}
    [:body
     {}
     [:div
      {:type "letter"}
      [:fw {}]
      [:opener
       #:xml{:id "xx"}
       [:dateline {} [:settlement {:ref "#STED"}] [:date {:when "1942-06-30"}]]
       [:salute {} [:persName {:ref "xx", :type "receiver"} "MODTAGER "]]]
      [:pb {:facs "24.628", :n "1"}]
      [:p #:xml{:id "p1"} [:persName {:ref "#np60"} "Jens Holt"] " , 16 "]
      [:p #:xml{:id "p2"} [:date {:when "1942-06-30"} "30 juni 1942"]]
      [:p #:xml{:id "p3"} "Kære " [:persName {:ref "#np60"} "Holt"] " ,"]
      [:p
       #:xml{:id "p4"}
       "Jeg tager fat igen idag for at skrive resten til dig af hvad jeg vilde skrive . Jeg har haft et meget anstrengende semester . De græske dialekter krævede et adskilligt større arbejde , end jeg i min letsindighed havde troet ; det er for længe siden , jeg har givet mig af med det . Dærtil kom et ikke ubetydeligt arbejde med finsk for viderekomne ; jeg vilde naturligvis ikke sige nej til de mennesker , da vi nu een gang var begyndt , og man vidste jo i lang tid ikke , hvordan det vilde gaa med den finske lektor ; iøvrigt var det jo , naar den finske lektor kom , ikke engang givet , at han var lingvistisk uddannet , saa jeg maatte stadig regne med eventuelt at komme til ogsaa i fremtiden at tage mig af de finskstuderende . Og for det tredie havde jeg – jeg kan stadig sige : i min letsindighed – annonceret forelæsninger over sprogteori , idet jeg havde regnet sikkert med , at min Sprogteori skulde kunne udkomme i begyndelsen af semestret . Hele januar arbejdede jeg dag og nat paa at gøre den færdig , og kom ogsaa et godt stykke , men ikke til afslutningen . Ved semestrets begyndelse blev jeg saa nødt til at smide alt andet til side , saa vidt muligt , for at koncentrere mig om den lille bog ” Sproget ” til fru "
       [:persName {:ref "#np16"} "Bodelsen"]
       "s serie , som det ogsaa lykkedes mig at faa færdig i løbet af nogle faa uger ; til min store ærgrelse blev det alligevel ikke til noget med trykningen : bogen var to ark større end stipuleret i kontrakten , og forlaget ( Busck ), som aabenbart i forvejen havde fortrudt , at de overhovedet var gaaet med til denne serie ( fru "
       [:persName {:ref "#np16"} "Bodelsen"]
       " , som er redaktør , vil gøre serien til en "
       [:term {:type "danishTerm"} "række"]
       " bøger forfattede af videnskabsmænd med førstehaandskendskab til sagerne og hvori videnskabens aktuelle problemer fremstilles for et akademisk dannet publikum med forholdsvis store forudsætninger – en idé , jeg ogsaa finder fortræffelig , de forskellige videnskabsmænd kender i virkeligheden alt for lidt til hinandens arbejde , - men forlaget havde tænkt sig en bredt anlagt meget populær serie , og det skændes de nu stadig om ), forlaget altsaa benyttede de to overskydende ark som paaskud til at nægte at tage den , med mindre den blev forkortet ; da jeg i forvejen havde meget lidt plads til raadighed ( 100 meget smaa sider til at fremstille hele lingvistikkens aktuelle problemstillinger for folk uden specialkundskaber !) og havde skrevet saa kondenseret som jeg overhovedet kunde , kunde jeg ikke gaa med til at forkorte . Da jeg har lovet fru "
       [:persName {:ref "#np16"} "Bodelsen"]
       " bogen , og da jeg for godt venskabs skyld nødig vil skuffe hendes forventninger og gøre alt det vrøvl , hun har haft , endnu værre , sagde jeg til hende , at jeg vilde prøve at gribe sagen an paa en anden maade og skrive en helt ny bog , der saa kun skulde fylde de ønskede 100 sider , men jeg véd ikke hvornaar jeg skal faa tid til det . Busck har iøvrigt sagt , at de nok vil tage bogen , men uden for serien , og til en salgspris af 10 kroner i stedet for 3 ; men jeg kan ikke offentliggøre den , førend det bliver mig klart , om bogen til fru "
       [:persName {:ref "#np16"} "Bodelsen"]
       " i dens nye skikkelse kommer til at afvige saa meget fra den , at den ene bog ikke laver konkurrence med den anden . Det hele har ærgret mig en god del , for ærlig talt har jeg uindfriede bogforpligtelser nok i form af ufuldendte manuskripter , og saa ligger her paa "]
      [:pb {:facs "24.629", :n "2"}]
      [:p #:xml{:id "p5"} "2 "]
      [:p #:xml{:id "p6"} "mit bord maaned efter maaned et fuldt færdigt manuskript , som ikke kan komme ud !"]
      [:p
       #:xml{:id "p7"}
       "Følgen af alt dette var , at jeg maatte begynde semestrets undervisninger uden at have gjort sprogteorien færdig . Og til sprogteori meldte der sig over tredive studenter ! Det var jeg naturligvis paa en maade henrykt for , men paa den anden side yderst betænkelig : jeg har tidligere læst over sprogteori alias glossematik uden trykt vejledning , og det har voldt deltagerne saa store vanskeligheder , at de fleste gik fra ; paa den anden side , tror jeg ikke , dette skyldtes emnet eller teoriens vanskelighed , men kun mangelen af et trykt grundlag . I "
       [:placeName {:ref "#npl1"} "Aarhus"]
       " spurgte jeg en dag efter en forelæsning studenterne , om der var noget , de vilde spørge om ; de sagde : Hvor kan man læse noget om det ? Og ulykken er jo , at det kan man ikke læse noget om nogen som helst steder i verden , i hvert fald kun i nogle stærkt kondenserede og for begyndere absolut utilgængelige tidsskrift - og festskriftartikler , hvis synspunkter endda for en del enkeltheders vedkommende senere er forladt. – Naa , jeg har nu klaret forelæsningerne meget godt , efter eget skøn ; et ydre indicium er , at jeg dog fastholdt over tyve tilhørere til semestrets slutning . Jeg har gjort alt hvad jeg kunde for at være letforstaaelig ; men følgen er ogsaa , at vi ikke er kommet ret langt , og at jeg maa fortsætte i næste semester. "]
      [:p
       #:xml{:id "p8"}
       "Den lille bog ” Sproget ” betød en stadig forsinkels ; for at faa den færdig maatte jeg i februar afbryde mine forelæsninger , og saa maatte jeg jo give studenterne kompensation i den anden ende af semestret og læste lige til "
       [:date {:when "--06-04"} "4. juni"]
       " . Det meste af tiden gik med at forberede min undervisning , og hverken "
       [:persName {:ref "#np116"} "Rask"]
       " - udgaven eller sprogteorien kunde jeg faa gjort færdige . Imens dyngede alt muligt andet sig op paa mit bord , og da vi naaede "
       [:date {:when "--06-04"} "4. juni"]
       " , var jeg ærlig talt dødtræt . Nu er jeg ved at komme paa ret køl igen og skal først afvikle en større korrespondance og andre smaa løbende sager , som stadig har maattet udsættes , og dærefter bliver det nødvendigt at tage "
       [:persName {:ref "#np116"} "Rask"]
       " op til forhaabentlig afslutning . Jeg bliver her indtil "
       [:date {:when "--07-20"} "20. juli"]
       " og tager saa til Raageleje . Hvor meget jeg naar i sommer , er ganske uvist . Jeg vilde forfærdelig gerne have i hvert fald sprogteorien fra haanden . Undertiden ønsker jeg mig at jeg havde 3 hoveder og 6 hænder. "]
      [:p
       #:xml{:id "p9"}
       "Alt dette tillige til forklaring paa , at du ikke har hørt fra mig . Det var med en vis fryd , at jeg i dit første brev for nylig konstaterede , at du mente , det var dig , der skyldte brev : saa vidste jeg , at du i hvert fald ikke var vred paa mig. "]
      [:p
       #:xml{:id "p10"}
       "Til næste semester fortsætter jeg altsaa sprogteorien , med eller uden bog , og har desuden lovet at læse over oskisk - umbrisk og over ungarsk . Forhaabentlig bliver der tid til i sommer at lægge det til rette , ellers bliver efteraarssemestret lige som foraarssemestret en karrusel , og det er ikke behageligt . Man troede engang , at en videnskabsmand og universitetsprofessor havde en fredfyldt og stille tilværelse med god tid til sine egne frie interesser ! – Jeg vil slet ikke tale om den tid , som medgaar til at passe Acta Linguistica , "
       [:rs {:ref "#norg2", :type "org"} "Lingvistkredsen"]
       " og dens Bulletin , fakultetet , de fonetiske laboratorier osv . Man faar først fred , naar man bliver emeritus ; men det morer "]
      [:pb {:facs "24.630", :n "3"}]
      [:p #:xml{:id "p11"} "3 "]
      [:p
       #:xml{:id "p12"}
       "mig jo alligevel altsammen , og det er jo hovedsagen . At jeg ikke i dette semester har haft nogen doktordisputatser eller embedsbesættelser , er et guds under , saa var jeg brudt sammen. "]
      [:p
       #:xml{:id "p13"}
       "Den finske lektor har vist sig at være en meget flink fyr , som vi nok faar glæde af. "
       [:persName {:ref "#np6"} "Hans"]
       " væsentlige meriter er ganske vist at han er løjtnant og skønlitterær forfatter , men han er dog tillige magister i finsk og har været ivrig for at forklare mig alt hvad han har læst og opgivet af andre finsk - ugriske "
       [:term {:type "danishTerm"} "sprog"]
       " og af sproghistorie ; han er i hvert fald lingvistisk orienteret ( hvilket naturligvis ikke er ensbetydende med , at han har noget begreb om moderne lingvistik ) . Og han er et pænt og fornuftigt menneske . Man køber jo i ret høj grad katten i sækken med disse lektorer , og særlig i dette tilfælde , da han var mig ganske ubekendt . Vi har forresten haft vrøvl lige til det sidste med denne sag , fordi "
       [:persName {:ref "#np51"} "Hammerich"]
       " havde faaet dekanen og fakultetets forretningsudvalg til at frafalde kravet om en forslagsliste paa 3 kandidater fra finsk side . Det skete under min protest , fordi jeg mener det principielt er yderst farligt at frafalde dette krav ; nu kan man altsaa komme næste gang fra tysk side og sige , at der er præcedens for at kravet frafaldes . Desuden var der fra finsk side intet oplyst om kandidaten , hvad der ogsaa skaber et uheldigt præcedens . Paa den anden side indrømmer jeg gerne , at kravet om 3mandslisten heller ikke giver nogen garanti ; fra tysk side kan man jo i hvert fald altid præstere 3 nazister , og saa kan det være nogenlunde ligegyldigt , hvem man vælger. "]
      [:p
       #:xml{:id "p14"}
       "Nu kommer der en lille historie om Høeg . Som jeg nok tidligere har fortalt , besluttede vi ifjor i "
       [:rs {:ref "#norg2", :type "org"} "Lingvistkredsen"]
       "s komité , at Høegs foredrag om de græske verbalabstrakter ( i anledning af din bog ) skulde trykkes i Bulletin VII , der for tiden er under forberedelse . Høeg fik 16 sider maximum og skriftligt paalæg af mig om at holde sig saa nøje som muligt til det mundtlige foredrag. "
       [:persName {:ref "#np6"} "Hans"]
       " manuskript , der er stærkt forsinket pga hans sygdom , hvilket vi har sanktioneret , er nu indløbet og fylder 26 tryksider i stedet for 16 , hvortil kommer , at hele første halvdel af manuskriptet slet ikke var i hans mundtlige meddelelse i "
       [:rs {:ref "#norg2", :type "org"} "Lingvistkredsen"]
       " , men nærmest er en gengivelse af hans opposition , og en ret skarp kritik af din bog . I det mundtlige foredrag gik han jo netop ikke ind paa din bog , med den begrundelse , at du ikke var til stede , hvad jeg fandt meget tiltalende og rigtigt . Men nu har han altsaa alligevel ikke kunnet dy sig . Han kalder dig bl. a. en proselyt i den strukturelle lingvistik , hvad jeg har nydt meget . Naturligvis har jeg standset sagen : vi kan ikke gaa med til , at medlemmerne trykker noget i Bulletinen , som slet ikke gengiver deres mundtlige foredrag ; og vi kan for resten heller ikke godt have angreb paa andre medlemmers videnskabelige arbejder , uden at de faar lejlighed til at svare , og det er der ikke plads til . Jeg bad ham dærfor forkorte , eller udtage det "
       [:term {:type "danishTerm"} "afsnit"]
       " , der ikke var berettiget til optagelse i Bulletinen . Begge dele erklærede han for udelukket . Han var forøvrigt aldeles immun for forstaaelse af princippet , men blev ved med at spørge , om "
       [:rs {:ref "#norg2", :type "org"} "Lingvistkredsen"]
       " da ikke sagtens havde raad til at trykke det , efter at "
       [:persName {:ref "#np104"} "Munksgaard"]
       " har foræret os 1000 kroner . Han kan ikke forstaa , at om vi saa havde 10 millioner , kan "]
      [:pb {:facs "24.631", :n "4"}]
      [:p #:xml{:id "p15"} "4 "]
      [:p
       #:xml{:id "p16"}
       "første halvdel af hans afhandling ikke blive trykt i Bulletinen . Han har sikkert opfattet det som en slags drilleri , men det maa han om . Det er alt for farligt , at medlemmerne ser at noget saadant lader sig gøre ; saa vil de jo hver gang komme med et manuskript der er dobbelt så stort og indeholder en hel masse som slet ikke var med i foredraget . Jeg stillede mig iøvrigt , synes jeg selv , meget velvilligt , idet jeg først tilbød ham offentliggørelse i ” "
       [:rs {:ref "#npub9", :type "publication"} [:rs {:ref "#npub9", :type "publication"} "Mélanges"] " linguistiques"]
       " ” ( et bind , vi harplanlagt for de extraordinære penge til afhjælpning af arbejdsløsheden ) og dærefter i "
       [:rs {:ref "#npub10", :type "publication"} "Acta linguistica"]
       " ; ” "
       [:rs {:ref "#npub9", :type "publication"} "Mélanges"]
       " ” vilde han ikke i , saa nu kommer hans afhandling i Acta , i septemberhæftet . Den hedder ” A propos d ’ une livre récent sur les noms verbaux en grec ancien ”, hvad der har moret mig , da han jo ellers hele tiden har været saa ivrig for , at det skal hedde ” verbalabstrakterne ”; han har aabenbart nu endelig indset , at det gaar ikke paa fransk . Da den er ret polemisk imod digi det omtalte første "
       [:term {:type "danishTerm"} "afsnit"]
       " , maa du have lejlighed til at svare , hvis du ønsker det . Naar korrekturen kommer , skal jeg sende dig et exemplar. – Han er en underlig fyr ; og det synes mig ikke saa lidt frækt at forvente , at jeg uden videre skulde være gaaet med til det. "]
      [:p #:xml{:id "p17"} "Nu mange hilsener . Hav det godt i sommer , og lad mig lejlighedsvis høre fra dig. "]
      [:p #:xml{:id "p18"} "Din hengivne "]
      [:p
       #:xml{:id "p19"}
       "Jeg har helt glemt at fortælle , at jeg hele vinteren har ligget i skænderi med kommunelærere i anledning af Schmidt - Phiseldecks ABC , som jeg i sin tid anmeldte i Politiken . Vi har haft sager indanket for Højesterets berigtigelsesnævn og mange ubehagelige affærer , ikke uden humoristiske momenter . Men det er en lang historie – og den har taget sin tid sammen med alt det andet !"]
      [:p
       #:xml{:id "p20"}
       "Jeg har haft nogle forfærdelig flinke og dygtige mennesker paa begynderholdet i finsk , og nogle af dem gaar nu videre med ungarsk . Det er meget morsomt , at der er kommet saa god gang i det finsk - ugriske , som altid har interesseret mig meget ; men der er dog ikke nogen af dem , der vil tage vores nye magisterkonferens ; det er fransk- , engelsk - og tysk - studerende , som morer sig med det ved siden af. "]
      [:closer #:xml{:id "yy"} [:salute {} [:persName {:ref "xx", :type "sender"} "AFSENDER "]]]
      [:postscript {} [:label {}] [:p {}]]]]]])

(def scrape-result
  (cup/scrape document
              {:all-tags '[tag ???]
               :p        '[:p {:xml/id ?id} ???]
               :rs       '[:rs {:type type} ???]
               :persName '[:persName {:ref ref :type ?type} full-name]
               :ref      '[_ {:ref ref} ???]
               :term     '[:term {:type term/type} term/name]

               ;; Deliberately bad results
               :glen     '[:glen {} ???]}))

(deftest test-scrape
  (let [{:keys [all-tags p rs persName term glen] refs :ref} scrape-result
        empty-map? (every-pred map? empty?)
        str-id?    (comp string? #(get % '?id))
        count=     (fn [n coll]
                     (= n (count coll)))]

    (testing :all-tags
      (is (count= 123 all-tags))
      (is (count= 54 (set (map 'tag all-tags))))
      (is (every? keyword? (map 'tag all-tags)))
      (is (every? (partial count= 1) all-tags)))

    (testing :p
      (is (= (count p) 22))
      (is (empty-map? (first p)))
      (is (empty-map? (last p)))
      (is (every? str-id? (butlast (rest p)))))

    (testing :rs
      (let [{:strs [org publication] :as groupings} (group-by 'type rs)]
        (is (= 4 (count org) (count publication)))
        (is (empty? (dissoc groupings "org" "publication")))))

    (testing :persName
      (let [{:strs [receiver sender] :as g} (group-by '?type persName)
            other (get g nil)]

        (is (= 15 (count other)))
        (is (= 1 (count receiver)))
        (is (= 1 (count sender)))
        (is (every? (partial count= 2) other))
        (is (every? (partial count= 3) receiver))
        (is (every? (partial count= 3) sender))

        ;; note: untrimmed strings, a consequence of the input XML file
        (is (= "MODTAGER " ('full-name (first receiver))))
        (is (= "AFSENDER " ('full-name (first sender))))))

    (testing :ref
      (is (count= 33 refs))
      (is (every? (partial count= 1) refs)))

    (testing :term
      (is (count= 4 term))
      (is (every? #{"danishTerm"} (map 'term/type term)))
      (is (count= 3 (group-by 'term/name term))))

    (testing "bad results"
      (is (nil? glen))))
  #_.)

(deftest test-get-bindings
  (are [pattern hiccup bindings]
    (= (cup/get-bindings pattern hiccup) bindings)

    ;; Catch-all
    '[_ {} ???]
    '[:p {} "text here"]
    {}

    ;; Shortened catch-all
    '[_ ???]
    '[:p {} "text here"]
    {}

    #_.))
