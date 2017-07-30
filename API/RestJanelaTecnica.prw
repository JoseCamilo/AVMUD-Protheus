#INCLUDE 'TOTVS.CH'
#INCLUDE "RESTFUL.CH"


//--------------------------------------------------------

WSRESTFUL VerificaFile DESCRIPTION "Verifica arquivo no server" FORMAT "application/json"

WSDATA Caminho 		AS STRING OPTIONAL

WSMETHOD GET  DESCRIPTION "Verifica arquivo no server" 	PRODUCES APPLICATION_JSON

END WSRESTFUL

//---------
WSMETHOD GET  WSRECEIVE Caminho WSSERVICE VerificaFile

Local lRet      := FILE(::Caminho)

If lRet
    Self:SetResponse(    '{"result":true}')
Else
    Self:SetResponse(    '{"result":false}')
EndIf    
    

Return .T.

//--------------------------------------------------------

WSRESTFUL EnviaEmail DESCRIPTION "Verifica campo na tabela" FORMAT "application/json"

WSDATA Subject 		AS STRING OPTIONAL
WSDATA Destnatario 		AS STRING OPTIONAL
WSDATA Body 		AS STRING OPTIONAL

WSMETHOD GET  DESCRIPTION "Verifica campo na tabela" 	PRODUCES APPLICATION_JSON

END WSRESTFUL

//---------
WSMETHOD GET  WSRECEIVE Subject,Destnatario,Body WSSERVICE EnviaEmail

Local cLog := ""

If findfunction("U_xSendMail") .And. U_xSendMail( ::Destnatario, ::Subject, ::Body,,.T., ,,.T.,.t.)
    Self:SetResponse(    '{"result":true}')

ElseIf findfunction("U_SIMMail") 
    U_SIMMail(::Subject,::Body,::Destnatario,"",{},@cLog)

    if Empty(cLog)
        Self:SetResponse(    '{"result":true}')
    else
        Self:SetResponse(    '{"result":false}')
    EndIf

Else
    Self:SetResponse(    '{"result":false}')
EndIf

Return .T.


//--------------------------------------------------------

WSRESTFUL VerificaCampo DESCRIPTION "Verifica campo na tabela" FORMAT "application/json"

WSDATA Alias 		AS STRING OPTIONAL
WSDATA Campo 		AS STRING OPTIONAL

WSMETHOD GET  DESCRIPTION "Verifica campo na tabela" 	PRODUCES APPLICATION_JSON

END WSRESTFUL

//---------
WSMETHOD GET  WSRECEIVE Alias,Campo WSSERVICE VerificaCampo
 
dbSelectArea(::Alias)


If FieldPos(::Campo) > 0
    Self:SetResponse(    '{"result":true}')
Else
    Self:SetResponse(    '{"result":false}')
EndIf


Return .T.

//--------------------------------------------------------

WSRESTFUL VerificaSX6 DESCRIPTION "Verifica Parametro" FORMAT "application/json"

WSDATA Parametro 		AS STRING OPTIONAL
WSDATA Conteudo 		AS STRING OPTIONAL
WSDATA Contspa 		    AS STRING OPTIONAL
WSDATA Conteng 		    AS STRING OPTIONAL

WSMETHOD GET  DESCRIPTION "Verifica Parametro" 	PRODUCES APPLICATION_JSON

END WSRESTFUL

//---------
WSMETHOD GET  WSRECEIVE Parametro,Conteudo,Contspa,Conteng WSSERVICE VerificaSX6
 
Local cStsBra := 'false'
Local cStsSpa := 'false'
Local cStsEng := 'false'
Local cStatus := 'true'

if GetMv(::Parametro,.t.)
    if valtype(self:Conteudo) != 'U'
        if alltrim(cvaltochar(self:Conteudo)) == alltrim(cvaltochar(SX6->X6_CONTEUD))
            cStsBra := 'true'
        else
            cStsBra := 'false'
        endif
    else
        cStsBra := 'false'		
    endif

    if valtype(self:Contspa) != 'U'
        if alltrim(cvaltochar(self:Contspa)) == alltrim(cvaltochar(SX6->X6_CONTSPA))
            cStsSpa := 'true'
        else
            cStsSpa := 'false'
        endif
    else
        cStsSpa := 'false'		
    endif

    if valtype(self:Conteng) != 'U'
        if alltrim(cvaltochar(self:Conteng)) == alltrim(cvaltochar(SX6->X6_CONTENG))
            cStsEng := 'true'
        else
            cStsEng := 'false'
        endif
    else
        cStsEng := 'false'		
    endif
endif

if cStsBra == 'false' .or. cStsSpa == 'false' .or. cStsEng == 'false' 
	cStatus := 'false'	
endif

cJson := '{ "status": '+cStatus+', "sx6":[ {"x6_conteudo": "'+alltrim(cvaltochar(SX6->X6_CONTEUD))+'", "status": '+cStsBra+'},{"x6_contspa": "'+alltrim(cvaltochar(SX6->X6_CONTSPA))+'", "status": '+cStsSpa+'},{"x6_conteng": "'+alltrim(cvaltochar(SX6->X6_CONTENG))+'", "status": '+cStsEng+'} ] }' 

Self:SetResponse( cJson )

Return .T.

//--------------------------------------------------------

WSRESTFUL VerificaFonte DESCRIPTION "Verifica Fonte" FORMAT "application/json"

WSDATA Collection 		AS STRING OPTIONAL
WSDATA Arquivo 		    AS STRING OPTIONAL
WSDATA ChangeSet 		AS STRING OPTIONAL

WSMETHOD GET  DESCRIPTION "Verifica Fonte" 	PRODUCES APPLICATION_JSON

END WSRESTFUL

//---------
WSMETHOD GET  WSRECEIVE Collection,Arquivo,ChangeSet WSSERVICE VerificaFonte

Local aArquivos := {}
Local nI := 0
Local aData := {}
Local cFonte
Local aFonte
Local lRet := .T.
Local aFontesRet := {}



aData := U_TDataArqTFS(::Arquivo,::Collection,::ChangeSet)
aFonte := StrTokArr(::Arquivo,"/")
cFonte := aFonte[len(aFonte)]

//verifica se o fonte existe no RPO
if len(GetSrcArray(cFonte)) > 0
    //verifica data e hora
    if GetAPOInfo(GetSrcArray(cFonte)[1])[4] < aData[1] .OR.;
        ( GetAPOInfo(GetSrcArray(cFonte)[1])[4] == aData[1] .AND. left(GetAPOInfo(GetSrcArray(cFonte)[1])[5],5) < left(aData[2],5) )
        lRet := .F.
        aAdd( aFontesRet, {cFonte,"Data Invalida no RPO"})
    Else
        aAdd( aFontesRet, {cFonte,"Compilado"})
    EndIf
Else
    lRet := .F.
    aAdd( aFontesRet, {cFonte,"Não existe no RPO"})
endif


//Retorno Geral
Self:SetResponse( '{' )
If lRet
    Self:SetResponse(    '"result":true,')
Else
    Self:SetResponse(    '"result":false,')
EndIf

//Retorno dos itens
Self:SetResponse( '"itens":[' )

For nI:=1 to len(aFontesRet)
    Self:SetResponse(    '{ "fonte": "'+aFontesRet[nI][1]+'", "status": "'+aFontesRet[nI][2]+'" }')
    conout(aFontesRet[nI][1] + aFontesRet[nI][2])

    if nI != len(aFontesRet)
        Self:SetResponse( ',' )
    Endif
Next nI
Self:SetResponse( ']' )
Self:SetResponse( '}' )


Return .T.

//--------------------------------------------------------

WSRESTFUL VerificaChangeSet DESCRIPTION "Verifica ChangeSet" FORMAT "application/json"

WSDATA Collection 		AS STRING OPTIONAL
WSDATA ChangeSet 		AS STRING OPTIONAL

WSMETHOD GET  DESCRIPTION "Verifica ChangeSet" 	PRODUCES APPLICATION_JSON

END WSRESTFUL

//---------
WSMETHOD GET  WSRECEIVE Collection,ChangeSet WSSERVICE VerificaChangeSet

Local aArquivos := {}
Local nI := 0
Local aData := {}
Local cFonte
Local aFonte
Local lRet := .T.
Local aFontesRet := {}

aArquivos := u_TListArqTFS(::Collection,::ChangeSet)

If Len(aArquivos) > 0
    For nI:=1 to Len(aArquivos)

        aData := U_TDataArqTFS(aArquivos[nI],::Collection,::ChangeSet)
        aFonte := StrTokArr(aArquivos[nI],"/")
        cFonte := aFonte[len(aFonte)]
        
        if !(upper(right(cFonte,3)) $ 'PRW/PRX')
        	loop
        endif

        //verifica se o fonte existe no RPO
        if len(GetSrcArray(cFonte)) > 0
            //verifica data e hora
            if GetAPOInfo(GetSrcArray(cFonte)[1])[4] < aData[1] .OR.;
                ( GetAPOInfo(GetSrcArray(cFonte)[1])[4] == aData[1] .AND. left(GetAPOInfo(GetSrcArray(cFonte)[1])[5],5) < left(aData[2],5) )
                lRet := .F.
                aAdd( aFontesRet, {cFonte,"Data Invalida no RPO"})
            Else
                aAdd( aFontesRet, {cFonte,"Compilado"})
            EndIf
        Else
            lRet := .F.
            aAdd( aFontesRet, {cFonte,"Não existe no RPO"})
        endif
    Next nI

    //Retorno Geral
    Self:SetResponse( '{' )
    If lRet
        Self:SetResponse(    '"result":true,')
    Else
        Self:SetResponse(    '"result":false,')
    EndIf

    //Retorno dos itens
    Self:SetResponse( '"itens":[' )

    For nI:=1 to len(aFontesRet)
        Self:SetResponse(    '{ "fonte": "'+aFontesRet[nI][1]+'", "status": "'+aFontesRet[nI][2]+'" }')

        if nI != len(aFontesRet)
            Self:SetResponse( ',' )
        Endif
    Next nI
    Self:SetResponse( ']' )
    Self:SetResponse( '}' )
Else
    Self:SetResponse(    '{"result":false , "itens":[]}')
EndIf

Return .T.

//-----------------------------------------------------------------------------
/*/ {Protheus.doc} Function

@author 
@version 
@since 
/*/
//-----------------------------------------------------------------------------
User Function TListArqTFS(Collection, ChangeSet)

Local oWsdl
Local aOps
Local xRet
Local aRet := {}
Local nI := 0
Local nPos := 0
Local cResp
Local aElem
Local cArquivo

Default ChangeSet := ""
Default Collection := ""


// Cria o objeto da classe TWsdlManager
oWsdl := TWsdlManager():New()

// Faz o parse de uma URL
oWsdl:ParseURL( "http://tfs-services01.sp01.local:8081/TFSWebService/ControleVersaoService.asmx?WSDL" )

if xRet == .F.
    conout( "Erro: " + oWsdl:cError )
    Return aRet
endif

// aOps := oWsdl:ListOperations()

// if Len( aOps ) == 0
//     conout( "Erro: " + oWsdl:cError )
//     Return aRet
// endif

// Define a operação
xRet := oWsdl:SetOperation( "ListArqChangeset" )

if xRet == .F.
    conout( "Erro: " + oWsdl:cError )
    Return aRet
endif

// Define o valor de cada parâmeto necessário
xRet := oWsdl:SetValue( 0 , Collection )
xRet := oWsdl:SetValue( 1 ,  ChangeSet )

if xRet == .F.
    conout( "Erro: " + oWsdl:cError )
    Return aRet
endif

// Envia a mensagem SOAP ao servidor
xRet := oWsdl:SendSoapMsg()
if xRet == .F.
    conout( "Erro: " + oWsdl:cError )
    Return aRet
endif

//trata o retorno
cResp := oWsdl:GetParsedResponse()
aElem := StrTokArr(cResp,chr(10))

nPos := ascan(aElem,{|x| left(x,8) == 'string:$'})
If nPos > 0
    For nI:=nPos to Len(aElem)

        cArquivo := StrTran(aElem[nI],"string:","") 
        
        If Left(cArquivo,1) == "$"
            aAdd(aRet, cArquivo)
        Else
            Exit
        EndIf
    Next nI
Else
    conout("ChangeSet sem arquivos")
    return aRet
EndIf

Return aRet


//-----------------------------------------------------------------------------
/*/ {Protheus.doc} Function

@author 
@version 
@since 
/*/
//-----------------------------------------------------------------------------
User Function TDataArqTFS(cArquivo,cCollection,cChangeSet)

Local oWsdl
Local aOps
Local xRet
Local aRet := {}
Local nI := 0
Local nPos := 0
Local cResp
Local cData
Local cHora

Default cArquivo := ""
Default cChangeSet := ""
Default cCollection := ""

// Cria o objeto da classe TWsdlManager
oWsdl := TWsdlManager():New()

// Faz o parse de uma URL
xRet := oWsdl:ParseURL( "http://tfs-services01.sp01.local:8081/TFSWebService/ControleVersaoService.asmx?WSDL" )
if xRet == .F.
    conout( "Erro: " + oWsdl:cError )
    Return aRet
endif

// aOps := oWsdl:ListOperations()

// if Len( aOps ) == 0
//     conout( "Erro: " + oWsdl:cError )
//     Return aRet
// endif

// Define a operação
xRet := oWsdl:SetOperation( "GetDateCheckin" )

if xRet == .F.
    conout( "Erro: " + oWsdl:cError )
    Return aRet
endif

// Define o valor de cada parâmeto necessário
xRet := oWsdl:SetValue( 0 , cArquivo )
xRet := oWsdl:SetValue( 1 , cCollection )
xRet := oWsdl:SetValue( 2 ,  cChangeSet )

if xRet == .F.
    conout( "Erro: " + oWsdl:cError )
    Return aRet
endif

// Envia a mensagem SOAP ao servidor
xRet := oWsdl:SendSoapMsg()
if xRet == .F.
    conout( "Erro: " + oWsdl:cError )
    Return aRet
endif

//trata o retorno
cResp := oWsdl:GetParsedResponse()
aElem := StrTokArr(cResp,chr(10))

nPos := ascan(aElem,{|x| left(x,len('GetDateCheckinResult:')) == 'GetDateCheckinResult:'})
If nPos > 0
    

    aAdd( aRet, CtoD(left(StrTran(aElem[nPos],"GetDateCheckinResult:",""),10)) )
    aAdd( aRet, RIGHT(StrTran(aElem[nPos],"GetDateCheckinResult:",""),8) )
    

Else
    conout("ChangeSet sem arquivos")
    return aRet
EndIf

Return aRet




//--------------------------------------------------------

WSRESTFUL VerificaAlias DESCRIPTION "Verifica e cria alias" FORMAT "application/json"

WSDATA Alias 		AS STRING OPTIONAL

WSMETHOD GET  DESCRIPTION "Verifica e cria alias" 	PRODUCES APPLICATION_JSON

END WSRESTFUL

//---------
WSMETHOD GET  WSRECEIVE Alias WSSERVICE VerificaAlias

Local cError := ""
Local oError := ErrorBlock({|e| cError := e:Description})
 
Begin Sequence
    DbSelectArea(::Alias)
    DBCloseArea()
End Sequence

ErrorBlock(oError)

If Empty(cError)
    Self:SetResponse(    '{"result":true , "msg":"Tabela existe"}')
Else
    Self:SetResponse(    '{"result":false, "msg":"'+EspecMsg(cError)+'"}')
EndIf

Return .T.


















// //--------------------------------------------------------
// //--------------------------------------------------------
// //--------------------------------------------------------
// // FUNCOES DE APOIO
// //--------------------------------------------------------
// //--------------------------------------------------------
// //--------------------------------------------------------

Static Function EspecMsg(cTexto)

Default cTexto := ""

cTexto := strtran(cTexto, CHR(1), "")   //
cTexto := strtran(cTexto, CHR(2), "")   //
cTexto := strtran(cTexto, CHR(3), "")   //
cTexto := strtran(cTexto, CHR(4), "")   //
cTexto := strtran(cTexto, CHR(7), "")   //
cTexto := strtran(cTexto, CHR(8), "")   //
cTexto := strtran(cTexto, CHR(10), "")   //
cTexto := strtran(cTexto, CHR(11), "")  //
cTexto := strtran(cTexto, CHR(12), "")  //
cTexto := strtran(cTexto, CHR(13), "")   //
cTexto := strtran(cTexto, CHR(14), "")  //
cTexto := strtran(cTexto, CHR(15), "")  //
cTexto := strtran(cTexto, CHR(16), "")  //
cTexto := strtran(cTexto, CHR(17), "")  //
cTexto := strtran(cTexto, CHR(18), "")  //
cTexto := strtran(cTexto, CHR(19), "")  //
cTexto := strtran(cTexto, CHR(20), "")  //
cTexto := strtran(cTexto, CHR(21), "")  //
cTexto := strtran(cTexto, CHR(22), "")  //
cTexto := strtran(cTexto, CHR(23), "")  //
cTexto := strtran(cTexto, CHR(24), "")  //
cTexto := strtran(cTexto, CHR(25), "")  //
cTexto := strtran(cTexto, CHR(26), "")  //
cTexto := strtran(cTexto, CHR(27), "")  //
cTexto := strtran(cTexto, CHR(28), "")  //
cTexto := strtran(cTexto, CHR(29), "")  //
cTexto := strtran(cTexto, CHR(30), "")  //
cTexto := strtran(cTexto, CHR(31), "")  //
cTexto := strtran(cTexto, CHR(34), "")  //
cTexto := strtran(cTexto, CHR(127), "") //
cTexto := strtran(cTexto, CHR(128), "") //
cTexto := strtran(cTexto, CHR(131), "") //
cTexto := strtran(cTexto, CHR(132), "") //
cTexto := strtran(cTexto, CHR(133), "") //
cTexto := strtran(cTexto, CHR(134), "") //
cTexto := strtran(cTexto, CHR(135), "") //
cTexto := strtran(cTexto, CHR(137), "") //
cTexto := strtran(cTexto, CHR(138), "") //
cTexto := strtran(cTexto, CHR(139), "") //
cTexto := strtran(cTexto, CHR(140), "") //
cTexto := strtran(cTexto, CHR(142), "") //
cTexto := strtran(cTexto, CHR(145), "") //
cTexto := strtran(cTexto, CHR(146), "") //
cTexto := strtran(cTexto, CHR(147), "") //
cTexto := strtran(cTexto, CHR(148), "") //
cTexto := strtran(cTexto, CHR(149), "") //
cTexto := strtran(cTexto, CHR(152), "") //
cTexto := strtran(cTexto, CHR(153), "") //
cTexto := strtran(cTexto, CHR(154), "") //
cTexto := strtran(cTexto, CHR(155), "") //
cTexto := strtran(cTexto, CHR(156), "") //
cTexto := strtran(cTexto, CHR(158), "") //
cTexto := strtran(cTexto, CHR(159), "") //
cTexto := strtran(cTexto, CHR(161), "") //
cTexto := strtran(cTexto, CHR(162), "") //
cTexto := strtran(cTexto, CHR(163), "") //
cTexto := strtran(cTexto, CHR(164), "") //
cTexto := strtran(cTexto, CHR(165), "") //
cTexto := strtran(cTexto, CHR(166), "") //
cTexto := strtran(cTexto, CHR(167), "") //
cTexto := strtran(cTexto, CHR(169), "") //
cTexto := strtran(cTexto, CHR(171), "") //
cTexto := strtran(cTexto, CHR(172), "") //
cTexto := strtran(cTexto, CHR(173), "") //
cTexto := strtran(cTexto, CHR(175), "") //
cTexto := strtran(cTexto, CHR(176), "") //
cTexto := strtran(cTexto, CHR(177), "") //
cTexto := strtran(cTexto, CHR(178), "") //
cTexto := strtran(cTexto, CHR(179), "") //
cTexto := strtran(cTexto, CHR(181), "") //
cTexto := strtran(cTexto, CHR(182), "") //
cTexto := strtran(cTexto, CHR(183), "") //
cTexto := strtran(cTexto, CHR(184), "") //
cTexto := strtran(cTexto, CHR(185), "") //
cTexto := strtran(cTexto, CHR(187), "") //
cTexto := strtran(cTexto, CHR(188), "") //
cTexto := strtran(cTexto, CHR(189), "") //
cTexto := strtran(cTexto, CHR(190), "") //
cTexto := strtran(cTexto, CHR(198), "") //
cTexto := strtran(cTexto, CHR(208), "") //
cTexto := strtran(cTexto, CHR(215), "") //
cTexto := strtran(cTexto, CHR(216), "") //
cTexto := strtran(cTexto, CHR(221), "") //
cTexto := strtran(cTexto, CHR(222), "") //
cTexto := strtran(cTexto, CHR(223), "") //
cTexto := strtran(cTexto, CHR(230), "") //
cTexto := strtran(cTexto, CHR(248), "") //
cTexto := strtran(cTexto, CHR(253), "") //
cTexto := strtran(cTexto, CHR(254), "") //
cTexto := strtran(cTexto, CHR(255), "") //

Return cTexto




// //--------------------------------------------------------
// // ESTE EXEMPLO UTILIZA O WS CLIENT COMPILADO
// //--------------------------------------------------------
// WSRESTFUL VerificaFonte DESCRIPTION "Verifica Fonte" FORMAT "application/json"

// WSDATA Collection 		AS STRING OPTIONAL
// WSDATA ChangeSet 		AS STRING OPTIONAL

// WSMETHOD GET  DESCRIPTION "Verifica Fonte" 	PRODUCES APPLICATION_JSON

// END WSRESTFUL

// //---------
// WSMETHOD GET  WSRECEIVE Collection,ChangeSet WSSERVICE VerificaFonte

// Local aArquivos := {}
// Local nI := 0
// Local aData := {}
// Local cFonte
// Local aFonte
// Local lRet := .T.

// aArquivos := u_ListArqTFS(::Collection, Val(::ChangeSet) )

// If Len(aArquivos) > 0
//     For nI:=1 to Len(aArquivos)

//         aData := U_DataArqTFS(aArquivos[nI],::Collection,Val(::ChangeSet) )

//         aFonte := StrTokArr(aArquivos[nI],"/")
//         cFonte := aFonte[len(aFonte)]

//         //verifica se o fonte existe no RPO
//         if len(GetSrcArray(cFonte)) > 0
//             //verifica data e hora
//             if GetAPOInfo(GetSrcArray(cFonte)[1])[4] < aData[1] .OR.;
//                 ( GetAPOInfo(GetSrcArray(cFonte)[1])[4] == aData[1] .AND. GetAPOInfo(GetSrcArray(cFonte)[1])[5] < aData[2] )
//                 lRet := .F.
//                 Exit
//             EndIf
//         Else
//             lRet := .F.
//             Exit
//         endif
//     Next nI

//     If lRet
//         Self:SetResponse(    '{"result":true}')
//         conout("result ok")
//     Else
//         Self:SetResponse(    '{"result":false}')
//         conout("result nok")
//     EndIf
// Else
//     Self:SetResponse(    '{"result":false}')
// EndIf

// Return .T.

// //-----------------------------------------------------------------------------
// /*/ {Protheus.doc} Function

// @author 
// @version 
// @since 
// /*/
// //-----------------------------------------------------------------------------
// User Function ListArqTFS(cCollection, nChangeSet)
// Local _oTFS := WSProdControleVersaoService():New()
// Local _aRet := {}
// Default nChangeSet := 0
// Default cCollection := ""

// If !Empty(cCollection) .And. nChangeSet > 0
// 	_oTFS:cnomeCollection 	:= cCollection   
// 	_oTFS:nchangeSet		:= nChangeSet
// 	_oTFS:ListArqChangeset()
// 	_aRet := _oTFS:OWSLISTARQCHANGESETRESULT:CSTRING
// EndIf	

// Return _aRet

// //-----------------------------------------------------------------------------
// /*/ {Protheus.doc} Function

// @author 
// @version 
// @since 
// /*/
// //-----------------------------------------------------------------------------
// User Function DataArqTFS(cArquivo,cCollection,cChangeSet)
// Local _oTFS := WSProdControleVersaoService():New()
// Local _cRet := ""
// Local _aRet := {}
// Default cChangeSet := 0
// Default cCollection := ""
// Default cArquivo := ""

// If !Empty(cCollection) .And. cChangeSet > 0 .And. !Empty(cArquivo)
// 	_oTFS:cnomeCollection 	:= cCollection   
// 	_oTFS:nchangeSet		:= cChangeSet
//     _oTFS:ccaminhoArquivo   := cArquivo
// 	_oTFS:GetDateCheckin()
// 	_cRet := _oTFS:cGetDateCheckinResult

//     aAdd( _aRet, CtoD(left(_cRet,10)) )
//     aAdd( _aRet, RIGHT(_cRet,8) )
// EndIf	

// Return _aRet