(ns advent-of-code-2017.day-4
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def input
  "vxjtwn vjnxtw sxibvv mmws wjvtxn icawnd rprh\nfhaa qwy vqbq gsswej lxr yzl wakcige mwjrl\nbhnlow huqa gtbjc gvj wrkyr jgvmhj bgs umo ikbpdto\ndrczdf bglmf gsx flcf ojpj kzrwrho owbkl dgrnv bggjevc\nndncqdl lncaugj mfa lncaugj skt pkssyen rsb npjzf\nkdd itdyhe pvljizn cgi\njgy pyhuq eecb phwkyl oeftyu pyhuq hecxgti tpadffm jgy\nzvc qdk mlmyj kybbh lgbb fvfzcer frmaxa yzgw podt dbycoii afj\nzfr msn mns leqem frz\ngolnm ltizhd dvwv xrizqhd omegnez nan yqajse lgef\ngbej rvek aehiz bgje\nyej cphl jtp swe axhljo ddwk obwsq mnewiwu klddd\nipiev henods rpn qfpg gjfdgs zcpt sswab eosdhn teeil\ngzje ydu oiu jzge udy sqjeoo olxej\nmgn gox tcifta vzc lxry gox gox mvila qdl jipjnw dvu\nhxk xhk unhdmdz yomze povrt nbww bxu qqsqc rvuk tgffy twddm\nfyx fyx nzkm fyx\nymnoc zogudq yncom tqrob sidvy dfuu ccjpiej yidvs\nbxebny akknwxw jeyxqvj syl cedps akknwxw akknwxw zpvnf kuoon pnkejn wqjgc\nkcebrkj zmuf ueewxhi mgyepbr nleviqc dez\nargavx fqguii gebohvw klnrq rkqnl goevhbw\nywqi abwi eswph nlplbl pswhe lnqx fpgk lllnpb\nabpb mpkw ampey yapme xnuyj\ntmuaq asd bhbs sqmbsnw wsbnqsm ydwdncn rpa vrllkh\ndnltf cck djy ydj\nwywwl scezo clowuz dkgqaj dohyzcp\ndiimshr vlmsnlj whqb dkicau ckdaiu terp kgcii npqc vvzrqzv nol\nwfpxe sqf tbb ruqpcq zfgb\nkajykuz tsxgtys vuz kglmgg ihnnn plyjxj rcrvo mij plyjxj jqiur\npxs hmet dwgvd mvhkvn cjxg yvid vmhnkv kwxz rfemsua wdgvd okixk\nlzwxas ddtyeh ivyama crrhxdt hedytd jfw\nvjfv oyd fvjv kfwlj mradbx mckseee xradmb\nllga yytxyvj lstspek lstspek lstspek\nfabgf wgop fabgf bvsfoaw\ngrqnbvo tntomdw hizg tmotdwn\nmau ufkw cxfi rhehj ebe xyv rhehj acxngo arl qtl rhehj\nkbkto stqjtm tpcwshj saerkrt pffj dthp pfjf axc gwmmfdw glnqtdy xmskw\nveff zqm hzhxap lgwnwq twsdk mqz xbbarbv cdx fhnwt qjcji bbvbrxa\nfjw eds hofskl nkbsv des hvx xyn\nqzort qzort qesz rtq oonk vwzlw wapoj ifr cta\npja hvy nhjg paj smtfe fmtse\nxvi tcjj xvkjtab nqftt aumijl xkd cmilegf hvsmodx uuo igmcelf mslkq\nmdhezgv lelzy kzfvsqu hvmvaxw pxiqjc hvmvaxw kzfvsqu\nhsicsav csshrhx znojm eapi lhmzq bbwnz seao gfk azk\npup xtgjyzy wqt ijeektl\nktwh qdegzs btj pfwzzho\nxdkmdm izqtjrr iqbke vtp\nfmrbpdr zpccv tmtwx tmtwx tmtwx bys\nehphfgq idd ehphfgq ehphfgq uphe hvrc jcscne nbnslqy\nxzqucgj fcih fljk barz lvln hcfi azrb\ncmfmclv mfgvifw rnxgn jpg bsnq wnduzj ymsdx smdxy pqomf\nrlqsm qrsml emts qsmcowv scmvwqo\ntshzkpa zwtpda ftsiwo nil tpawdz kjpa ptzashk\nmnep sfc swjawtd vnwud gyulluw zpa kmwyvln evd btnmoi dnwe\njwq scepq redoxmw rbdzsa wlkzso kxpm bttg vxuc moxwdre ijtdd rzsabd\nwpvo dsjox amuwjm pls lgwksva ctakgpl rmsjj lzwwpr zzm udg\nbji obbn tmwyc afpmkxr glvrd kahhgpq rna qkxyntp vmd mloshc\nymq rtnr nxjzm pqiddrn qmy vgxw ull\nmmzk ikge zhtzhs xyo qwe lll gjjm icetq qgrr mzwqa knec\nkxomfck idlh xrbowo nyetbnl qskh xuwkkxe upmmmf zhvuyp\nsrcwyhl czgr xmhuws jueyh xcuib xhsuwm bxuic\ncrkueh beyxopz xpyozbe dxgadw qktmce rjropjg\nlbktun imdpcp fkssp fhcpt fehho jqdnt aoewa\njmun pynzjo trs ijwcc pelf oft pcsqdxg zvql\nmneaaq vjrg jidlrzz phd mvxpivd ldkhu\nsao xqw nrukn gatkz quscpsx vmz oscoeb\ngoi wzxhb rrk aylqqcd mlcbvvf ororn heptid kdu byevr\nqsj lsbieef deez vzwdx hez iwd\nlmgfb keqt mqbsuis ogrr errbi xiqe xsszacp\nato hmk zfjaj kmh plxup cida dqd pfwh nkbxvpr buajw pxkrvnb\ncli bdwu vrwott vowtrt grle\nzisgks ciuaqr zvk tcb kvz ugmtv\noegrojm wofpwp gnaocx rweyull ellhwow dtefylf dqsz oiw varr bcirpf oxusz\noydkmib oydkmib yefts gbl gbl\nsruwjk pgkrp kea gppkr zdcky cfljh\nobpxbax jhpcrj slcsa lgd fborz vvpaus wsrpsws ifijuzo\nrixz jwh uhdaf hoacv hdfua\nkntk qprmfow kntk tbmcjx\nvnqe ooyxtb ixl hdmnpn orpz ykspl xromvj kowtq wmho gquos\nynk xjjqw sut lmtub bmtlu zdo dztlk bpkuul smhpx rbczg\nzals csdbe sbj dibicq kdfwwt\ncoyy pjddlfc lwvhyms ldjdcfp ryubz kfwst dqjrjja jtv jjjaqrd\njaexhms iqoiln ewgyr exmnrr fsr lgmyy fdofhn\npjgyn hfoz zbcnz nczbz\novntivq vcey vdrkse giu ohyaxy ionyy fvpn yvwrgrv qta\nyelpz htbk njgeyub tggh mdthzp fwyux rduqli twlhfp pnh gywif ttn\nyxhsbil vplsmmx rgtq grsf lyibxhs hctnkfr awg lmloz jroy lpgb wga\nkzytass szyksat tyskasz ehmhhu\njkus hwjv ymnnkk yffugg cvtnits gbl lywkn szihcn dbrbalf rxqpbqh\nkoyfcef wkom mwok qgjrytl\nslmhry lcr slmhry lcr\nmvoxbt cfkz purnsui xar ouhtc thbx\nxcdifw kvvxyrj knac qmypw bou tmukqy eusgaoo bktiu\nablgnhb axumg bwpxnjp zqpc vtw ghhoxu zqpc znfpvl ghhoxu jlg ntdk\nvmvc cdkhrx cvz rvxk mmcuo udpcayd lsmm gufduzt linj\nmgyeqkv hqionh rgnqgz kkc qrgnzg egkmqyv topdp\nkoa dimwx gjxa atlfdy\nuuez ueuz zeuu ezo daq\nofpaw bgomvmt mqa dexpy mbipd epyzcoa nuwrh vwly xppz qkjrleo rwhnu\nwok grxk lchvtg plrzr lwaax cfeu ijapws dmkdwc cfeu\nzkd hysxxip hlydw wicsvy gbwoaw dapre ktjn dzg uri\nblzh hblz qgmjceg fyf\nvkhpn xnc ogva pjrh cxn hkpnv\naja cldzta tdcazl lorr fwmxxh knilf ges tdhp gnlo vihrl\nucpr peair nlbmc msfg\ntrv ppq bmo xqd vbui yegsr xqxawu fvuz aclhspo wnan\nloiq fvg kare rmgq hir rzo ossd ziw renh ygtkjys vda\nxmans kio alexs ujekfl vvf ddghn\nfcxvsf bjuytet zrzsobo uhn mlfzhlq bjefs\nzys htlqvky plno pbcqfuf fjwc vshkxrl lonp lyzmy dqmui vyyc glad\ntlc krhcter krhcter bolk tlc opryl\nidcii dverl uswb wusb zgax zhbt gjsnlso yhs\ncti npri rcbxjdw ollj nirp ghfvxzh\nblyhug aflnrrz zudyw ccnstq cyoju jxtqoj ntuknjq gunjiwy ycuoj igac cqctns\nbul yehpnw jifjrhc ifetu ufrodp hqzpeqf hdvpc qtvgxg ibb wcxsitx xztshb\nxzct scetn eoaufyo jtudgkx xrpgxip lpubtq juezstc nuc hokswh obkf ipbu\nnfq lwpmn qltal xnphsqs zlrgf iewtrtd mqzsob duokpy kfbqs icg\nvil zjz xkqrvni uay ystq\nterrrnt lnfg clm lbs ptpiy ybcuup ayzjm pqugx lmc yppit mbf\ndtajh vqivg vnblt fmn qxkw stiwna pclrrr fro khu wbslnqp tjyosu\nuqlehn tjuiy obt uedct bbwiq uxndqn\nhiqfovy xiimca zwne ivunvjk cmctzi mxnnrx dclib xzaoq ieztkg\nshpr xuorihj chuwq poadbo mhtvex gymsp iltgl sypjfua fmyh sgiv\nalv nxjt txnj bhact\nvjvtrex obmrxk fgigs meixbc fggsi awi rxdjpeg\nypwo oicmbdw xbpeeyj uabzj cjvutvc oicmbdw immtmks\nexijri hogl epr gzdqyur xiiejr pre ihzlgzu\nrlh qfhx lrh qmvrx\nkogq okhd mivmivb mivmivb okhd\ntaekt nhjaa znbaahn iaospxy jawwf\nytdvq ghtqwud jkiig mre kzmmjxu jba nwpykc\nktyzr aczd exgadhb uinrgac izazxky yyfe\nyrifb qgc lsiuapg teyelxn ugezu\nwdzkc ltx fkhncb hwrecp kfbchn sfcpc hjvq\nrjdjyt ahwxh nvggsmx lmz oshd xbcik powse ahhxw yhiq gxmgsnv\nqdr qjnam gag qjamn kooek mqnaj\npza gml opf ilfbblu kjp luilbfb rhfrzgp ixagj ofp\nyphz runy dhull bozcsgk wfxekrd akgkbz urcphc\ntfyxwol lhcl npik beug\nszatel yfkve yfkve lzqhs\nyjzqon pcjibu bdncmcl kczuymm pbmg nyn\nrerqvs aoxucwi pmstl sstawu joqu abvcchg mvgjn mslpt vhmfkr utusuh\ngqbec jjpqdh yeaiavi nledfi jhzwc vyxjpf momnm vnknjs nvgjzik ipm\npsirt rispt lrkgma irtsp\njbbaph xvunete gsvnr mjd ifxhpry cpsx hmuokkx vhcm yth shrrl zbhd\ngfa bcmlxtf sqyanrp cugg qxfvftz pbl ujsgc jajxltm gugc oil\nxjuhyg aht vmyvzhh oby oyb ybo xbybgmx\natfk qjudfzz mky tfy\nnxk yzy jqgg qxgjt bevvvv efi xcbw bohc zaqlqjq\nhdc qpnx ygmtqw acvoa udboxw dhc klh mwgpk xfpuri\ncycgbkq skwhyf skwhyf veaqss skwhyf\njnezf jowjt vsdu uck scgxd fvopomz vfajslp\ndjvi epgkyqn apzd cpm owm kpwih fsr adlhqu jicp pmc\nerxlmhj wqxvofi ugj ttrmtsb\nomku vmrgoy tdicbje ewml dfnwbap\ngpih pyt ptsmzc gmdbu rqxkqmz objm nurxjz oozbere ztxug koth\njpnl jpnl dmeh qed\nintdwv ksgw qwlzhq zpd lrl mwjl dozrjwq aujbet bsnf vhqyg\neqs uot qyz xor aem kmrh mrhk jqx tsbrf\nirytjab mdzm qbb kkjt gofiwo xgbovg kyeyxqn tcks tljhx\nzgejy qodgah nqavvx xnigdvt\neqve bizrxq lkhz yzwxgt nwe zfe sxypkz xnssept\nbxqn lkfg yfxbszo sphwifz wnj crhbq dvokzw\nvzn afatwye ogzvnu vnz rfjba xtugnj kpbgly ocsjd\nxrc cxr rahv yvhk khyv bed ctgbuq cmqwpqa jlbg hpj vmesvw\njbshkya dgqw lfl mzcch jxsg czcmh ifruvlw ufwrlvi xcczlol cqqchmr\nrbk mhn tnmqdc sxnnn kvoa mhn sxnnn mgemob ieiyajs\ncqi ghxg ghxg ghxg\nuqwdxn qli gdtkngp gnptdgk udxqwn\ndmcczr dnjaqc qwdta rhrbi hkdwe qdjcan peic iulaz xns\ntcmppb nzq ecy sitdud nft ecy afrbf wvnc vmfpzx tcmppb cgb\nplitv efnpq mjqav nrxxo izg lpitv rwbzdo rdbzwo\nday dntga adtng agndt hhvtd\nyrg iudsh gyr ryg\nqttyeew tco flq bszw jkzftc wdh efcwnp mja rfmju\nmoch prkze uslzyv plhjuy kxczyq qlmm hgq\nxtg ypz izy ixg bvs xlqgj xcy sepza abiylsg\nwxvsxn bqag jnlzgxq ikxwa dfd plqxl xlgqnjz nuqvoyb emhodso gaqb\nbzjdsm xmxkj fhuqn gauyw ntl kjxmx zcxdr vrds\nofjcc uxyzlk ofjcc ofjcc\nzwosex kkvwobl cpudsmb kes zklf bayuojr otqnyr udbbs\niqpvzh ybds piovrh oivprh voprih pov sfl\nupns cpeelht xboyk itb hsxdmt dnwgfbw upns fygf kwdpxzm mli dyy\ndjwutl sikh shki ikhs gecd jqkon trqyw\nprbbdf vdp bvvfjcg ydqb muxygg\nvhpurzn psemqe xwqfk hrvonxu nxkxacq\nxicmhss tnpja qiad woipfy uvadcq usljh hzgs jntvfv wzikk\nmmupc twntp upcmm pumcm\nqnisuzy lppnfd uiqr eyqbain uxlp eyrfwjo olgkrps sbikam zin vckr\nnmokl skfni jcdfot njzqeaj nqzjjea\nslmaxx offfzqp wudicrf nfn rwfcdui cwirufd\npaffi murnjd oyj lbtjdqe babuas dtqh qkt stapzl yrqlp\needc rig zmnfmn edec ecde\nbcfdf edovdj lacx nzvze sordvxj ybs ujh zvvvp rzstejg ueosuq\nxrrfsd okuvem znzlvmb jwzcb bfg bmuxbc qzwfry\npqgxybd cvgra acgn ocd ancg fvfcx fbb bfb zfzv\ntmmv mpywyg fwl bnvcv lcnv flw\nxxnfbro papc ianru beuzx apcp rnt\nwuyhycj nrnc cka ebg rncn rvo wcyhjuy\nthh cmoog hwf imqfp okzpxd\nrzxiqt rtaiy ytria tyria\ncjkmro myif myif xyirn aqxlol wlhwibi dhzsen pzwgm bfbz bufjs qwffg\nmxhiui umiihx zomyll vfieccs\nyyntf rjk iivgj mwh rjk\ndsshx wsmaxhc xcwuelh rdsgtr wsmaxhc rgtsfj\nrdh nwlxiwu xsjzbpr bsgps\nufyo vqtzkg kpeohu mxzt fyuo gawgaq youf\nhzbhut bxsnjwb zuhhbt zhhtbu\npdz sgntypg ragev hrrji goitft yphnebs xjzoo sqf jsuzijq dsocb hcxg\npptsq woomypc woomypc woomypc\naxcg wfbnpql ejqb cmnn nncm csvlc wraludb pkmp whtht tfpicer\nmoom oomm ommo vfqeii\nxvrgpp rofl yxyrkb oage nypzau pwfnkn jxnhkw cyxsi clzb adwpuh\nmfbz vdtt muzhm wvwwfl ttdv\ncpqgvbu byc pgfrlkr aftl tqm zcqxi juu gnf ppovxh huoa\nkonpcp lzordid jqng lwxs nqgj gghkxmf kyn ngqj\niorhccj xfygc cnfr tysqc xpcyf vmjpitf nut zmrk mgbrtb tcblxwf dkadwrm\nkov jtmp xoatesx qxkilp rmggpfx ltpxzwf vko reqms mqq nps\nhjigmk fyqy wpuwe mwmso thsimfs okcmeyh mzqkez duzaq vzhyrm uyvpkox cwivpls\nukoerf korufe zhs ntwfz hugem vriyk enfaib hrrcdgf zllsk vkiyr\nshkx khxs wntpjv qdevaw noqyht nwpvjt egh hgok mukdjfi law bzbvjz\ndquk kczxsq tdu trnkjs wqtdc ybvcb\nhlrotxn cumcjkm qwufgle ylm nejh hnje pvaigrx myl sfvsd\nszmvisn aywic vsnimsz iufmybr\nzjozr zojzr qmn ffrggdh wam dafvok\nnxkvlhr posmf posmf posmf zhlzb\nywis kpqpyb qae zqxpuz pcj hbsfz ejlwa lajew znuom\nqxsl ussivur dstd avojo\nyoeagao egpaqm ymzf kkauy ivm illir wsvchne skmamvn nqxc\ncldo ixzzy vhk nra zhypgab\nqjdd ecxud tbuqq mpotbdk tjdpczn knncm tyy\nrbfc fhhjf innia tsjbbbv fmtcuup rapvhqz ebpzt whdbms gvjoy lykl fquvcby\nbihhfwi lhal udxz uwjwp dmb\nfekxamy uophet yzvv rqj zawlp ldrv mdymkzy taauf\nrcwxvmh edueui ltdyo xfghz dgjig senm ifj\nqcu fii axmgijj ifi oixjfsg jxagijm\nsdtyr rbdh yvnvq czzuig wro\nlot xkto cmpiena nht ozcg aotcw xiegl cyaouj und lsclep cexn\npgihljk cmgmv sajhi zfvbqij ogwoc ajsih zmppe\njexwkdp dwpexjk mzjydfu bff rubgdb\nyshfhx emkl hshxyf mkle\ndxgti jdo tkwprv pbxbrqd oiz gsbdphd qotu utfdnq tzvve bqc\novdf bshfxyl xspjpd vljdsm mgkd djlsvm mlsjdv\netyia eytai sfq qafj xzgp ewhsn snwhe lhqp\nzjz mwh dorxm ges gexo rckwsa dltoq mmntha\nhqkuj ypsjcxo dixbe rmvnhjh ovnr\nedc iffaxc lolu xwrvpb gva vti vit\nceuxq xbwejr lzyvm rozseit cwe mham fivpwj qtv omaktaw\nalzdrk tsxbuld mdbq pgbdtoo xwf vzalric nqe jqwlxsy cbtylu dtubxsl lqm\nrqjmjcs exjpn kpilcgu ihcm lfadjm mlri hpd vqs cxqwqhu twxrtk\naeuvlcp aubvnw riedvz arypagp uuvg kliehx cokt ogh xsdw cdsyywv\nddwrgvp bscaq bbfv qrbutp\njpdg uey eyu uyarl zgbk qyhqq fdvlql zmwkp\nkbt bkt lebhpfu smrzt xalw mmwa zmtzfry tkb\nfcvcv oewfzu fvvcc mldww lwdmw\nejrltsu sqoyx wfvsdbp bfdspvw bfir jqhgrmt ofdmrjg ysq\njzwucwn erqjd eikq knpf cvk xvqnscy eei wvfjzmj xujq cqaim boev\njqhkmr ipjpj zwno ybu krhqjm zqfyyzb dyciy\nugwsw rpwteje qtvwi pwyhrzt hfcdfmc qbovk ibws\nffy kdder qjookz bfvmvvq yjzuaj fvxllfb pjyz jcezhye fimyydt qjookz qjookz\nloupd nwsc yytvuqo ltcqxnf\niho ulvxguz fxbf iqu ofjtmvq xhs ybbusd kxg mebdnah ucttcf zufb\nwzdb wumuhtv kef aavv buu xmjtlur faaccl wospwff bjasr eapfsi\njau qzszci ciu inagax\nkui tqig fyovsp fvwol fyovsp mzth tcp nhoq\najdla wtpj amylu jly tvq wjqef\nofqc einz bdze tows bdze eew\navwavzt aesrsjv lbmpi hllv chdbul ezelxn\nimcprs cafb clfg rsjo iylqu nvk vkrq izezlnu vkqr tyhnv\nrwj zboui reh buzio wuhpvid cpzy jrw tsbuiby hmxwqr ute\nixq luwbi uoiwsjh souz ysoubw uilbw ffwjvw ewzswoh hci zmfdaov whowzse\nxrhgqf xrhgqf giyv giyv\ntoiqgzv gakg udgdlb wvi carrn pjyha muqclu\nwuxthi srtszr ourab hpds bakvy fnk yefe yfee doowxcx\nijdc ujhvls xmy hwg yetsda qelbe nang xgywo wgh\nbhm icq cnam dec enksf qfctz pwxoo bdf cnma xoowp rbls\njguzh fextz yax kesaunn waljo jltcza tfzxe dezs syi ebwxnks\nflvq bzgd clvqw ahtyvu xbdyv wssxx boscm grgl nqcg\ncaskpli hqctxxc nwpyo wjlqfqf ebti dva\nwmsz fzpd ikgeq gti ejftoou ezs cqef mybojc rgwz\nmdaay yfppa pavl fuuvfkh hpod tpb dhxmia emdecm rbqcwbk vecyt\nneha rmvl ndp vlrm dpn debghi vyhvc\nbnp zkxdu iqqkesd abtlx hmjasdq kyvekr krt srrjyd oxmfev oot\ndumlcqd ccm hyir oritdz madjjw\noakqrs advfmu verrc zkfdcn btndsp\nonlkinl rdtm bscfxre bnu oumyrvv kgc zkj\ntfxfsgm uwmic agswclg uofezgc\nwpfdyjn kjlihk etbot fbu scm gwccce xgownte wig cuaijbo\nbzbdk etozk qracb oftfoo lkooe\nxupzw vmxwu sis wzpxu\ngbz oqbgh jwgrru bzg kwmxcfc jrurgw\nagyjnyc tuec imxlult omwiyjg fiwnoqx nhmnro qtg kbr agyjnyc\nkoiq llreotu elrtoul dubfvgy whq\nhtm lll crzppb gdjaae nsmxzh gnfvn obiuy ymspzbo iuboy\nthm xlfrr pbxdfo mht tygi sapxgbv mmngzf dej\neus seu qmstw ues\nyvfsw esut biblze kbjcpk estu xih qzki ezlbbi blzv\nohq ugc tqqeo jygvpwm vfs ldnfibp ycbpa sml rmime\nkuuow gbg nzwdaf wiimtg lam oqmm\nwsbwkdd hda nqk ticz mvt\ngqbljyh zqugqs cjod sxwlqy qkfs wwvwvt dsojb qbhjlgy riusoa uosari\njkphfx dbt les jsvoij rnuw mxmmchu dol vto swn\nqqxe vwvephr twdqlyg cvdu xjiych clooq vkwavl whvverp yuz vkwval\ntxtbudi tiutdbx wqhx tws utgbf amh hmf izsez ooz\negdube nhsxjs nxjshs xoy sjsxnh\negdziod diodegz ejxn zogedid uhhkr rnm cyvvuc uqbl\nrbn pinwag sidwdwv jqdbe jlbemk blkeaqq ipfqbtn zkrbp\nbdryz sbh wxvn mhot wemsfm oemkff\nvxyn xvdwwo xhd vyca zxjaw vfkz xhg ofsphks dyq mmzzd\nyjrqsjf iiesdh envwyx rmtbmiv ggzsg ukx bprfym qmyqc vag ymho hjtoh\nfuxxrd wbweptd vkoffr wbweptd\ngfwcez smetli yjyh pslpz qyokpsm qsy cxjymg wqfkf obuq awz\neqhm ceest kayf heqm\nrdi dti vntcf ewkmpvf jjwoihc\nsfq qlb xrm ocy vtnj zdznbal zvon stln zwnj wsgalvq vhphap\npya jay mgnyo pya xmapdn\nhrwbj xhr gvwl ktq ktq gvwl\nrzgqi hjwtthl kxhggbl wepc hgavj ctmqug\ntzfwkc xeqfath iiuwq iiuwq dhwuvy\ngibagy smq getjofc lum msq ulm xuxu bilrus ily\nxlv ndrkch hdcknr nqltoze xvl\nwmc vuzlrj mwc atp cvpx atv ujatz\nhxpafgl ymjltv nvvpy ahycdk jhpdcks ettm lvqyw ertpivm dnezwxx usi kdhcay\nvrh hqyomv mcq ilwjbkz yprjxad\nugv szfitxg zeluib pfj ijm zmiigxx gltxzz jzljhgh otskue\nmxp bilj jlbi tce yfted zxsqas ftyed\nykasqv ehye kirmnl upmi dojwmw wzj ykasqv ifixn vreoypz\nkerbgub nnroqk onkqnr gbebkur tjhl knjo ccsem yozvrcg\nygq evkoj wkn ffljhds scxeibh egsybeg mwvi vgjblj qda ywqpp\nhocvpl ozgkxp xgmj ejzyxm\ngernu kks lxe nxzv sypg xle goz\nxoatis fjp wzlbo dzkonz jtutyj vdonj swro tqclemv xhomap ymeqkua vaxcw\nmxcyjs ywyxndk wng vpftv nsuvu\njmiyyhh gwser shgcu jmyg cjzegc hmhe eopg kmkan\nsmdd dmds mgqhtkh qtamih haqmit skkcy\ndnj rmggy rgymg uburbao rymgg\nklcpjgq ons ajyv sqryt son pjlcgkq xlobdt\npiw shonk tzi mcdumz noskh tebolw yaypn\nozm mvmjgtg nxj weommiq asnmhzq xjn uobztuo cqgjh utfb oydt ommiewq\nqlwgsc vvpe xgft ahpjc zjtx iyof scwqlg dxgcokx ltrefj xyzq rwto\nggqdd dqgdg ggdqd kjkmmfp\nhtzjam fjbg iagc xls iagc iydtf ihxl boa iydtf\nvhe nqj bwgdoi hhaoa qtulz\naxvyja hpdkwee hnryj prou rgadv oubjdqg knjbc\ncaz xibj wqkzwe peioeya vmz hesy ftb\ndudwcr gupj sjrtzc xsqbb hiet nujv bebcvsj eks uuzlcx gex\nkywozi tfzuc mflssw hnxxxqt zzc tzfuc hkokuv mnjg lwkavjp lvpwjak xez\nizgh zfv cingjt dkf cknite qox vfz zvf\nojpu dzk tehpgnt gntpteh\nglxfxa uxq ajtles ahgzn ajlste zwgc mrpu adz wuunwhc zda\nhdgdtn hnoyz aromkb qujfv yjgmn tbf atw\nuyvsv oaopjv uyvemxk ldpp tthe iisjk txr hebmd yxevukm rkziao znt\nypdr mnwuzvw acpg kzwz ywbn wcrr umrnlbe lkult ljify azyhu mgqoo\nabmpl omsd xmyl mxyl mgoq kracrf ufm ppwi zpggh\nuxfdpv jnm vvc vchunhl ubv ktj mxolsxz\nfcja eci edzrb nlvksaw lhf ycohh tfztt xso ceub tyv\nrkwtp tcmmvv kufg cxui hdamg suuaej fgku cvjlv\noldbgy riadoyo djsi wca zxoeq pmemqap aijxa\nnyy ruxcosx xisqoz yny jvzfpbe tlfdiaj ybd jifatdl zuzv\nkxwdz qvrvx svllp ergmme\nswjfuv eronk favcxfm acptbh pnbjn ciqcrlt rgvdnlt icgahb\nddza xxfn use obqka bfzwjp gmf bld fyvde mxdfdl\name bmxbyf ame bmxbyf\nrdgby pyfog dybrg gdryb lpztd\nsntg impd uxgxai naoalb ntnk xgix\noadpmqj oso criln izih oos\nouzjq gtl ito xefqt phnv ouzjq hoyjjj\nmlp rboq lpm roqb whvp\ntghcw ggshevw dzsgj ggshevw kec ggshevw\nkmwhb kfcb mbhkw gemz fdh\neuve veue kplrq evue\nhikfiw bcdktj hcnawja gjasvwc vcht igrzly rkxijxe ikfwhi dvmp\nhvksis kafs ktcs sfyqzyt etctrgt vodwr wff tskc juobnm\ndpcsodn ehwc pglywfl yhdp mdiyzx\nibog umftejh cfm pnxhna wqwx yabnk ygws dqw\ndezz tqw qism rarfe fpmlab xvbau irwtfs wwmoyss yvn xetqp xtqep\npchqwk npsmd jefec qok uuc ucnpz rlkakn\nkudh rjysb xrdbx bkbmjfo xrdbx\nrogu ssdwsus voa ncw obkxsr\ntflf hlevus scq rrbpat tau wxsq wxoblt\nrzr lex kqdy whtj ffnys xlgkkff msjhy dimaq hrc wyde qkwf\nghtwd wernjpn tdgwh olrfvmr edq gxvp\nrjirvf skhdgln aauit bipu mubjiwp kowz gyjfbjx cmgdqs\naftfpbv agajyy aqjll vsf twh robpys lebt eav yribup\nsby ymkla sxkbfwl awmd nhb vlp\nkizvjj ycjswr jkzjiv vuy jijzkv jcs\ncwvch xzqfal tephz lqfzax cnkbdcr mql zflaxq\njjxzwl himpra ssjf bibfiui seeaq pzse\njogrn jogrn sqew jogrn oixgwr\nkhonpyw iiyxir vybhc ndnxxv kzlt ipmncn\nokqkqu svbemi nfn ovd xgwy edd ujet nrrbv dde vdo\njobvf dus asvio vaosi sovia\nknmz qbz nkmz zmkn\nisbmopr unduey impobrs hea zswciev sopbmri duuj\nocs ntgnrdu kbvtzp cvyieu fiyn znmh lhrz ixtnzrj vktbpz lbpqx vzkpbt\nmuduhc sabc dlyoisz kuaz ogpyepw yuog ictiiqt\nxjflsf nfklvml thfh uajnmby cichyj xxoqi lpime bxpyx\nriahifn bohbgd obhdgb jni qzvkf ybp hjkkwq ytutd cakcsh smfdoe tuytd\niddku nccp zgtl yne ppzpqcx lwm\nrefpcz uqt uqt uqt\nmtn czxkagb nmt caqacrg bcakxgz\nitxjii uethxbj vpds bsqod diqax inv zrwt doepe\nbfyaj nbvhg zmi buf\ndtre dkwdr nrapm qtfth odvt bbcnae vxuk gqm enlg\nybt qcfozrk yzrh bfp euuozuz pzsdkxx mhi nbkzprb\nvpuhqn gyx caint antci vfep incat kqdakdx\nddhi chgnjk ibg xbemitr mjtdph eovw\nngbtuvq qdttlsg dbqhhwk bkrqze qdttlsg qdttlsg\nevn smvhi dgcmn xjo ascc ahbpj uvzc pwn tung\nksu thr omg onvsqzz rllakar ysfjtfj grxwyx oawix gpk suk\nqvb iouav yhtndkd vuoia ouaiv\nkud kofcip hcczrgc cvvxxlk rvyamwe duthdzr dftun\nrgv ynw gph tmxwfup nwy\ndnc trawj kwzbx trawj zvp\nogqxijy tbqtsg tbo vqinnlq jbvgl sfafh rve mcxqs ubh\nqccr lpv puuvdyb tydaflf uxic\ntlon tbfwkxg tlon tlon\niytiz qjlqaqw uixb lnt zwro uzgxqfi gklgnqs zwgoidw iifk wkwdo\ntmvhxw tmvhxw tmvhxw fhiqpjy ejk kvysd\ncmphg xjjz groiccd dvetuk xbwa zhm lyi ohhd neg bxaw yil\nkdmzopy lxx bvhach goxmxu qbqvzcm qbbrhvb nrfom aixmio grpxz hbrqbbv lkucih\nbnqn phqr uycuxc mopyyfh bbpesqm stgigq stggqi cwtjm asqhpl imvlxj lbmloo\npws iuvbvjr cwccm qbr srqnstz cjebq\nbfh jobkcy gtbroe lpagq icmax jobyck fbh\nounqdo qrrr pwi alho rrqr beao rsioepe\nvrccqge qvcgrce cbslkjs qnclw rvmjkw\naaxjns deupjs wtgxtp penad depbho tbrdt depbho qxg zhjxpgd\ndrqfo kbp jfa jaf\nizn oczcitj cpae quvzqo iwwk jck idjdpm\necort zgcvxx bvh vrprsf\nfhubfvy ndcfjo kol hyufbfv hvpka\nkpt zgajpc rjvsxa gayznjd\nxeoixk peq kfu lqa mjnv mzvh bicl hlfk\nwyt imdx lksy twy\nxeptp ilxs qbsqzwn rsy slxi xtpep dsdkekl\nrotvbt fuirp elos ciu nhx bxej trmtx ixn xbpc vrxtma\nskcprn yns sao ghlq vftezvc aaryahy telt\nfkaov gexa xijv yiksa xega dhgw okfva gxxs edkecag mqbqvrm nrzcqub\nljc jujxeof fdj gdzjzr mabbktu pmyrfv uspven zxry snt hrah\nnhujhdr jdhrnuh midm bbavhpp cpjk zmpbasz eptrpou znq zqn\nywzfq wuu lfflon uuw rke qzwyf hjbms gakx\nyqrq zsk jzn uuuzrml kzs lseupsg waynfh blech\ngwyqej weyjqg uwuje uujwe\nlxud rnwkc bgygkh csq rfvtos ystqp keb gkakodj uthcce eqxifl\nelvj evj rfwo vvgkosh aarcgjs utsbh orwf jxcqvmh uowmktl qtgf\nbqszre oxntty ombwiz mbiwzo\nccp iilcc tacf czk giwv erqi jgdfah wip xtrzhv wosvbyb\ngymyw rwsxeg gvydr izyk spsonkg knospsg\ndjj tbr tbr tbr ice\nyyzh zkykapw puydtik ysxc hjumhsd cuhhw dnnhida yyzh lnklymg\nnhbcxsu ccrbbyw scbxunh ghxrkqh brcwcyb\nlatdaav sexa ipzuzjl ayusb etb fshh\ngiz akqd vjmabii arfuzgv efrww jxkvolg efrww vrnzgbx\njmcc vqy adkzj fqrkdo tjrczp ccmj cfponk rptzjc\njsviu sraw imsj fujm cdf xwqhl lhz ojejzuy trtqblg\nibz dulm muoq quom etvjzxn tuhrpp jfukac jqctqn qhgbae msgmcit ludm\nzgx bpfa elhp rnyqtq wyceube nkeuxz\nlzxfo vygpecv jszacku zfxlo\ncpmv ysaaj xnp wbvqg hrsiuj venjxna yeqvwmk ftaga dcqxc jgapb rqdixp\nxpbbe tyn hfdlu fto wrgzkou sxylv cqto wdv xqc pnu rapk\npkrxypl wnu oipq tzbhnc gpug tgzf ofjb\nmvaz bwcv gll itgcye dessw szt gzimgeu bvmohh wbywyhc kzerxbr anjsive\nlhvnrzs qkmjwy pnyciwp mgp jfdz ghvtf yusfzg upab\nxbscukx aubulj snbcmc uscxkbx ddpucyg\nhgv ollh yzpjmpy fcicyae vhg gvh\nprd onyd iux oik xui\nzipadig nvewx cir lbpcusx dljqy\nifyxzsc btmy lsu tmyb lus ldyzx\negmyxbe ieasvek dylmj qahtatr uyqgbk\nmejjczw spj vaekp kdud\nvwan mgenld mnlged vpfuil euoxlr rclkpi dfknyoa rhthij kcyxl qaxab crlpik\npqm eihogk iwml nuauxi ngilkoh jmu mbdi cqxz nblb rmuj zczdgp\npswbe mtzch wbeps fxtnc psa aioff pas\nprwrpvz oadpqvz tgzrt giom pjyihh rxdir dmya xjolzxv\nkhdybe obqkjn kdq jkvmgwo enpat wyw qjbnko waid msest wwkoyts\nyep liv ofmtpod imdd qyw\nafnrx jgn gxarpb myltj ggrsajy mdaobjo vbtn vbtn zlziz eds\nhqr kqu oub skoeqk icnfm cqvld aay bto\nrga odaf exoosh pwevx zpbd plaa xoseoh\nmbr gqu oxvchrt nqa larxmjx pfozej\nozuo ywubjbg xcua eblwqp nfdvw hmhen zkjfu gmhgp bsyi ktprtf\nsrc vrysby srybvy znwjm hmypwdl gdmau pqe\ncldr crhi lbaq fbuduyn hygbz uhida\nqrxukq dygkp oaks soka oask\nvpido ajgfq pwlv hezt fmg epwrxo rqvjke iovpd hhkjm\nanxf ydl xnfa hqph olorp\nexydcg onxjm psqlbv ehz boar hze qsblpv\nmnzrvc ipj swg ijp sgw gdkntsd fzz grqwly\nerpq qghpj fay gci uglm afy\njwbq hbxaub jpdilyt yvalrlk topl qup\neczonk ftcc paltirb owz tihhe dglxory wthvqcb qdnxm lirejh alyxsr\nooruaby gboyeu lkv arrz jcqyzl uxlfk fhmeony fcmh\nwzr xjb pwmf okqj adwcedy lkidve uwekxf asbdzr biub\ndikhur pxgh urdinjh wednf ulzdxs\niplf byt tyt qnnlba pzt bednml ljjtkvo tjovlkj uwms xat\nhtzk ltmfha xikeze atfmhl fchxhyz\nlqala bqwgcul vetaa xuxjau zcb wtdmomu wfqmpq sief uyblyz ahv\naytvvo awm ojaaigg awm dbfaokz\nabq npcyld fzbfku oia qss jkxldm wgtmki pasgxi dieix rpqnuac tecnfy\nnmr qzfj qjfz lsz vnahex\ndjxoo jzlkh svy xige\ntjlkkg glcuvmh fwzlhi ecun qlgulj hrfhyql qgdlf ofakqdf zokkvm gelxkq oowgs\nupfpk gfstjlv lxc rjd nhj sbq jpzsz zsjzp\nfavd nzqfdid nekfjsf mtjndu\nsgdqx uvpuefv vhwrgd aivav gsqxd jdhfoq\nllaf cthbgy njrpw fqgkx jzf xqkgf lnrfrm gkxqf\nwzdwlc wisst alw kyjeur sjsqfcr tta bijnyn whfyoxl\ndtjr baxkj lmnyrlg nrmyllg\nmtgky xmwf zdko nnocxye gytkm ygp hixk xwmf\nmaudjy okgjga uadjmy dzfrk omd\nazz ajdcqkd bcafn zaz dcjaqdk gylyzo\nxzvfbf fopmfxu mvftgr mfupoxf coyhof talcc vpkslo")

(->>
  input
  str/split-lines
  (map #(str/split % #" "))
  (filter #(apply distinct? %))
  count)

(->>
  input
  str/split-lines
  (map #(str/split % #" "))
  (map #(map sort %))
  (filter #(apply distinct? %))
  count)
