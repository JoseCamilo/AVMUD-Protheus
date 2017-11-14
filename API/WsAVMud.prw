#INCLUDE 'TOTVS.CH'
#INCLUDE "RESTFUL.CH"

/*/
    Abrir e salvar este fonte com a Encoding UTF-8
/*/

//-------------------------------------------------------------------
/*/{Protheus.doc} VerificaFile
Verifica a existencia de um arquivo no server
@author jose.camilo
@since 29/07/2017
@wsmethod VerificaFile
@verbo GET
@receiver Caminho do arquivo que sera verificado
@return logico + mensagem
/*/
WSRESTFUL VerificaFile DESCRIPTION "Verifica arquivo no server" FORMAT "application/json"
    WSDATA Caminho 		AS STRING OPTIONAL

    WSMETHOD GET  DESCRIPTION "Verifica arquivo no server" 	PRODUCES APPLICATION_JSON
END WSRESTFUL

WSMETHOD GET  WSRECEIVE Caminho WSSERVICE VerificaFile

    Local lRet      := FILE(Self:Caminho)
    Local cMsg      := ""
    Local oJson  := JsonUtil():New()

    If Empty(Self:Caminho)
        SetRestFault(400, 'Caminho do arquivo não informado') 
        Return .F.
    EndIf

    If lRet
        cMsg := "Arquivo existente"
    Else
        cMsg := "Arquivo não existe no server"
    EndIf    
        
    oJson:PutVal("result",lRet)
    oJson:PutVal("msg", cMsg)
    oJson:PutVal("obj",{})

    Self:SetResponse( oJson:ToJson() )

Return .T.

//-------------------------------------------------------------------
/*/{Protheus.doc} EnviaEmail
Envia um email pelas funções do Protheus
@author jose.camilo
@since 29/07/2017
@wsmethod EnviaEmail
@verbo GET
@receiver Assunto + destinatario + Corpo do email
@return logico + mensagem
/*/
WSRESTFUL EnviaEmail DESCRIPTION "Envia um email pelas funções do Protheus" FORMAT "application/json"
    WSDATA Subject 		    AS STRING OPTIONAL
    WSDATA Destinatario 	AS STRING OPTIONAL
    WSDATA Body 		    AS STRING OPTIONAL

    WSMETHOD GET  DESCRIPTION "Envia um email pelas funções do Protheus" 	PRODUCES APPLICATION_JSON
END WSRESTFUL

WSMETHOD GET  WSRECEIVE Subject,Destinatario,Body WSSERVICE EnviaEmail

    Local cLog := ""
    Local lRet := .T.
    Local cMsg := "Email enviado"
    Local oJson  := JsonUtil():New()

    If Empty(Self:Subject) .Or. Empty(Self:Destinatario) .Or. Empty(Self:Body)
        SetRestFault(400, 'Assunto, Destinatário ou Corpo do email não informado') 
        Return .F.
    EndIf

    If findfunction("U_xSendMail") .And. U_xSendMail( Self:Destinatario, Self:Subject, Self:Body,,.T., ,,.T.,.t.)
        lRet := .T.

    ElseIf findfunction("U_SIMMail") 
        U_SIMMail(Self:Subject,Self:Body,Self:Destinatario,"",{},@cLog)

        if Empty(cLog)
            lRet := .T.
        else
            lRet := .F.
            cMsg := cLog
        EndIf

    Else
        lRet := .F.
        cMsg := "Nenhuma função de envio de email está compilada"
    EndIf

    oJson:PutVal("result",lRet)
    oJson:PutVal("msg", cMsg)
    oJson:PutVal("obj",{})

    Self:SetResponse( oJson:ToJson() )

Return .T.

//-------------------------------------------------------------------
/*/{Protheus.doc} VerificaCampo
Verifica a existencia de um campo
@author jose.camilo
@since 29/07/2017
@wsmethod VerificaCampo
@verbo GET
@receiver Campo que sera verificado
@return logico + mensagem
/*/
WSRESTFUL VerificaCampo DESCRIPTION "Verifica existencia de um campo" FORMAT "application/json"
    WSDATA Campo 		AS STRING OPTIONAL
    WSDATA Empresa 		AS STRING OPTIONAL

    WSMETHOD GET  DESCRIPTION "Verifica existencia de um campo" 	PRODUCES APPLICATION_JSON
END WSRESTFUL

WSMETHOD GET  WSRECEIVE Campo, Empresa WSSERVICE VerificaCampo

    Local cNomeCampo    := "" 
    Local cAlias        := "" 
    Local cTipoCampo    := ""
    Local lRet          := .F.
    Local cMsg          := ""
    Local oJson         := JsonUtil():New()
    Local cError        := ""
    Local oError        := ErrorBlock({|e| cError := e:Description})
    Local cBkpEmp       := If(Valtype(cEmpAnt)=="C",cEmpAnt,"")

    If Empty(Self:Campo)
        SetRestFault(400, 'Campo que será verificado não foi informado') 
        Return .F.
    EndIf

    If !Empty(Self:Empresa)
        RpcClearEnv()
        RpcSetType(3)
        RpcSetEnv(Self:Empresa)
    EndIf

    cNomeCampo    := readValue('SX3', 2, PadR(Upper(Self:Campo),10) , 'X3_CAMPO') 
    cAlias        := readValue('SX3', 2, cNomeCampo, 'X3_ARQUIVO') 
    cTipoCampo    := Upper( readValue('SX3', 2, cNomeCampo, 'X3_CONTEXT') )

    If Empty(cNomeCampo)
        lRet := .F.
        cMsg := "Campo não encontrado no dicionário de dados"

    ElseIf cTipoCampo == 'R' .or. Empty(cTipoCampo)
        
        Begin Sequence
            DbSelectArea(cAlias)
        End Sequence

        ErrorBlock(oError)

        If !Empty(cError)
            lRet := .F.
            cMsg := EspecMsg(cError)
        Else

            If FieldPos(cNomeCampo) > 0
                lRet := .T.
                cMsg := "Campo existe fisicamente"
            Else
                lRet := .F.
                cMsg := "Campo físico não criado na estrutura da tabela"
            EndIf

             DBCloseArea()
        EndIf
        
    ElseIf cTipoCampo == 'V'
        lRet := .T.
        cMsg := "Campo virtual existente no dicionário"
    EndIf

    oJson:PutVal("result",lRet)
    oJson:PutVal("msg", cMsg)
    oJson:PutVal("obj",{})

    Self:SetResponse( oJson:ToJson() )

    If !Empty(Self:Empresa) .And. !Empty(cBkpEmp)
        RpcClearEnv()
        RpcSetType(3)
        RpcSetEnv(cBkpEmp)
    EndIf

Return .T.

//-------------------------------------------------------------------
/*/{Protheus.doc} VerificaParametro
Verifica se o conteudo de um parametro esta correto no ambiente
@author marllon.fernandes
@since 29/07/2017
@wsmethod VerificaParametro
@verbo GET
@receiver Nome do parametro e o conteudo em Pt Eng Spa
@return logico + mensagem + estrutura com valores atuais e status individual
/*/
WSRESTFUL VerificaParametro DESCRIPTION "Verifica Parametro" FORMAT "application/json"
    WSDATA Parametro 		AS STRING OPTIONAL
    WSDATA Conteud   		AS STRING OPTIONAL
    WSDATA Contspa 		    AS STRING OPTIONAL
    WSDATA Conteng 		    AS STRING OPTIONAL
    WSDATA Filial 		    AS STRING OPTIONAL
    WSDATA Empresa 		    AS STRING OPTIONAL

    WSMETHOD GET  DESCRIPTION "Verifica Parametro" 	PRODUCES APPLICATION_JSON
END WSRESTFUL

WSMETHOD GET  WSRECEIVE Parametro,Conteud,Contspa,Conteng,Filial,Empresa WSSERVICE VerificaParametro
 
    Local lStsBra   := .F.
    Local lStsSpa   := .F.
    Local lStsEng   := .F.
    Local lRet      := .T.
    Local cMsg      := "Parâmetro com dados atualizados"
    Local oJson     := JsonUtil():New()
    Local oConteud  := Nil
    Local oContSpa  := Nil
    Local oContEng  := Nil
    Local cBkpEmp   := If(Valtype(cEmpAnt)=="C",cEmpAnt,"")

    If Empty(Self:Parametro)
        SetRestFault(400, 'Parametro que será verificado não foi informado') 
        Return .F.
    EndIf

    If !Empty(Self:Empresa)
        RpcClearEnv()
        RpcSetType(3)
        RpcSetEnv(Self:Empresa)
    EndIf

    If !Empty(Self:Filial) 
        cFilAnt := Self:Filial
    EndIf

    if GetMv(Self:Parametro,.t.)
        if valtype(self:Conteud) != 'U'
            if alltrim(cvaltochar(self:Conteud)) == alltrim(cvaltochar(SX6->X6_CONTEUD))
                lStsBra := .T.
            else
                lStsBra := .F.
            endif
        else
            lStsBra := .F.		
        endif

        if valtype(self:Contspa) != 'U'
            if alltrim(cvaltochar(self:Contspa)) == alltrim(cvaltochar(SX6->X6_CONTSPA))
                lStsSpa := .T.
            else
                lStsSpa := .F.
            endif
        else
            lStsSpa := .F.		
        endif

        if valtype(self:Conteng) != 'U'
            if alltrim(cvaltochar(self:Conteng)) == alltrim(cvaltochar(SX6->X6_CONTENG))
                lStsEng := .T.
            else
                lStsEng := .F.
            endif
        else
            lStsEng := .F.		
        endif

        if !lStsBra .or. !lStsSpa .or. !lStsEng 
            lRet := .F.
            cMsg  := "Parâmetro com dados desatualizados"
        endif

        oConteud := JsonUtil():New()
        oConteud:PutVal("x6_conteud", alltrim(cvaltochar(SX6->X6_CONTEUD)))
        oConteud:PutVal("status", lStsBra)
        oConteud:PutVal("msg", If(lStsBra,"Conteudo Português Atualizado","Conteudo Português Desatualizado"))

        oContSpa := JsonUtil():New()
        oContSpa:PutVal("x6_contspa", alltrim(cvaltochar(SX6->X6_CONTSPA)))
        oContSpa:PutVal("status", lStsSpa)
        oContSpa:PutVal("msg", If(lStsSpa,"Conteudo Espanhol Atualizado","Conteudo Espanhol Desatualizado"))

        oContEng := JsonUtil():New()
        oContEng:PutVal("x6_conteng", alltrim(cvaltochar(SX6->X6_CONTENG)))
        oContEng:PutVal("status", lStsEng)
        oContEng:PutVal("msg", If(lStsEng,"Conteudo Inglês Atualizado","Conteudo Inglês Desatualizado"))

        oJson:PutVal("result",lRet)
        oJson:PutVal("msg", cMsg)
        oJson:PutVal("obj",{oConteud , oContSpa , oContEng})

    else
        oJson:PutVal("result", .F.)
        oJson:PutVal("msg", 'Parametro não existe no ambiente')
        oJson:PutVal("obj",{})
    endif
    
    Self:SetResponse( oJson:ToJson() )

    If !Empty(Self:Empresa) .And. !Empty(cBkpEmp)
        RpcClearEnv()
        RpcSetType(3)
        RpcSetEnv(cBkpEmp)
    EndIf

Return .T.

//-------------------------------------------------------------------
/*/{Protheus.doc} VerificaFonte
Verifica se o artefato commitado no TFS, esta atualizado no ambiente
@author jose.camilo
@since 29/07/2017
@wsmethod VerificaFonte
@verbo GET
@receiver Arquivo: caminho completo no TFS do artefato commitado, Collection e changeset do TFS referente ao artefato
@return logico + mensagem + estrutura com status individual por fonte
/*/
WSRESTFUL VerificaFonte DESCRIPTION "Verifica Fonte" FORMAT "application/json"
    WSDATA Collection 		AS STRING OPTIONAL
    WSDATA Arquivo 		    AS STRING OPTIONAL
    WSDATA ChangeSet 		AS STRING OPTIONAL
    WSDATA Ambiente 		AS STRING OPTIONAL

    WSMETHOD GET  DESCRIPTION "Verifica Fonte" 	PRODUCES APPLICATION_JSON
END WSRESTFUL

WSMETHOD GET  WSRECEIVE Collection,Arquivo,ChangeSet,Ambiente WSSERVICE VerificaFonte

    Local nI := 0
    Local aFontesRet := {}
    Local lRet  := .T.
    Local cMsg  := "Fontes atualizados no ambiente"
    Local oJson := JsonUtil():New()
    Local oItem := Nil
    Local aObj  := {}

    If Empty(Self:Collection) .Or. Empty(Self:Arquivo) .Or. Empty(Self:ChangeSet)
        SetRestFault(400, 'Caminho completo, Collection ou Changeset do arquivo que será verificado não foi informado') 
        Return .F.
    EndIf

    aFontesRet := StartJob("u_VerFonte", If(Empty(Self:Ambiente), getenvserver(),Self:Ambiente) ,.T.,Self:Arquivo,Self:Collection,Self:ChangeSet)

    If ValType(aFontesRet) != "A"
        SetRestFault(400, 'Erro ao conectar no ambiente informado') 
        Return .F.
    EndIf

    If Len(aFontesRet) > 0
        //Retorno dos itens
        for nI:= 1 to Len(aFontesRet)

            If !aFontesRet[nI][3]
                lRet := .F.
                cMsg := "Pelo menos um dos fontes estão desatualizados no ambiente"
            EndIf

            oItem := JsonUtil():New()
            oItem:PutVal("fonte", aFontesRet[nI][1])
            oItem:PutVal("status", aFontesRet[nI][2])
            aadd(aObj,oItem)
        next nI
    Else
        lRet := .F.
        cMsg := "Nenhum arquivo valido para verificar neste ChangeSet"
    EndIf

    oJson:PutVal("result",lRet)
    oJson:PutVal("msg", cMsg)
    oJson:PutVal("obj",aObj)

    Self:SetResponse( oJson:ToJson() )

Return .T.

//-------------------------------------------------------------------
/*/{Protheus.doc} VerFonte
Função auxiliar, para veficar data no RPO atraves do arquivo, changeset e collection
@author jose.camilo
@since 29/07/2017
@function VerFonte
@receiver Collection e changeset do TFS referente ao artefato
@return artefato e status
/*/
User Function VerFonte(cArquivo,cCollection,cChangeSet)

Local aFontesRes := {}
Local aData := {}
Local cFonte
Local aFonte

//Funcoes auxiliares para resultado do TFS
aData := U_TDataArqTFS(cArquivo,cCollection,cChangeSet)
aFonte := StrTokArr(cArquivo,"/")
cFonte := aFonte[len(aFonte)]

//verifica se o fonte existe no RPO
if len(GetSrcArray(cFonte)) > 0
    //verifica data e hora
    if GetAPOInfo(GetSrcArray(cFonte)[1])[4] < aData[1] .OR.;
        ( GetAPOInfo(GetSrcArray(cFonte)[1])[4] == aData[1] .AND. left(GetAPOInfo(GetSrcArray(cFonte)[1])[5],5) < left(aData[2],5) )
        lRet := .F.
        aAdd( aFontesRes, {cFonte,"Data Invalida no RPO",.F.})
    Else
        aAdd( aFontesRes, {cFonte,"Compilado",.T.})
    EndIf
Else
    lRet := .F.
    aAdd( aFontesRes, {cFonte,"Não existe no RPO",.F.})
endif

Return aFontesRes

//-------------------------------------------------------------------
/*/{Protheus.doc} VerificaChangeSet
Atraves do changeset, Verifica se os artefatos commitados no TFS, esta atualizado no ambiente
@author jose.camilo
@since 29/07/2017
@wsmethod VerificaChangeSet
@verbo GET
@receiver Collection e changeset do TFS referente aos artefatos
@return logico + mensagem + estrutura com status individual por fonte
/*/
WSRESTFUL VerificaChangeSet DESCRIPTION "Verifica ChangeSet" FORMAT "application/json"
    WSDATA Collection 		AS STRING OPTIONAL
    WSDATA ChangeSet 		AS STRING OPTIONAL
    WSDATA Ambiente 		AS STRING OPTIONAL

    WSMETHOD GET  DESCRIPTION "Verifica ChangeSet" 	PRODUCES APPLICATION_JSON
END WSRESTFUL

WSMETHOD GET  WSRECEIVE Collection,ChangeSet,Ambiente WSSERVICE VerificaChangeSet

    Local nI := 0
    Local aFontesRet := {}
    Local lRet  := .T.
    Local cMsg  := "Fontes atualizados no ambiente"
    Local oJson := JsonUtil():New()
    Local oItem := Nil
    Local aObj  := {}

    If Empty(Self:Collection) .Or. Empty(Self:ChangeSet)
        SetRestFault(400, 'Collection ou Changeset dos arquivos que serão verificados não foi informado') 
        Return .F.
    EndIf

    aFontesRet := StartJob("u_VerChaSet", If(Empty(Self:Ambiente), getenvserver(),Self:Ambiente) ,.T.,Self:Collection,Self:ChangeSet)

    If ValType(aFontesRet) != "A"
        SetRestFault(400, 'Erro ao conectar no ambiente informado') 
        Return .F.
    EndIf

    If Len(aFontesRet) > 0
        //Retorno dos itens
        for nI:= 1 to Len(aFontesRet)

            If !aFontesRet[nI][3]
                lRet := .F.
                cMsg := "Pelo menos um dos fontes estão desatualizados no ambiente"
            EndIf

            oItem := JsonUtil():New()
            oItem:PutVal("fonte", aFontesRet[nI][1])
            oItem:PutVal("status", aFontesRet[nI][2])
            aadd(aObj,oItem)
        next nI
    Else
        lRet := .F.
        cMsg := "Nenhum arquivo valido para verificar neste ChangeSet"
    EndIf

    oJson:PutVal("result",lRet)
    oJson:PutVal("msg", cMsg)
    oJson:PutVal("obj",aObj)

    Self:SetResponse( oJson:ToJson() )

Return .T.

//-------------------------------------------------------------------
/*/{Protheus.doc} VerChaSet
Função auxiliar, para veficar data no RPO atraves do changeset e collection
@author jose.camilo
@since 29/07/2017
@function VerChaSet
@receiver Collection e changeset do TFS referente aos artefatos
@return lista de artefatos e status
/*/
User Function VerChaSet(cCollection, cChangeSet)

Local aFontesRes    := {}
Local aData := {}
Local cFonte
Local aFonte
Local aArquivos := {}
Local nI := 0

aArquivos := u_TListArqTFS(cCollection,cChangeSet)

If Len(aArquivos) > 0
    For nI:=1 to Len(aArquivos)

        aData := U_TDataArqTFS(aArquivos[nI],cCollection,cChangeSet)
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

                aAdd( aFontesRes, {cFonte,"Data Invalida no RPO",.F.})
            Else
                aAdd( aFontesRes, {cFonte,"Compilado",.T.})
            EndIf
        Else
            aAdd( aFontesRes, {cFonte,"Não existe no RPO",.F.})
        endif
    Next nI
EndIf

Return aFontesRes

//-------------------------------------------------------------------
/*/{Protheus.doc} TListArqTFS
Função auxiliar, para listar artefatos commitados no TFS atraves do changeset e collection
@author jose.camilo
@since 29/07/2017
@function TListArqTFS
@receiver Collection e changeset do TFS referente aos artefatos
@return lista de artefatos
/*/
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

    // Define a operacao
    xRet := oWsdl:SetOperation( "ListArqChangeset" )

    if xRet == .F.
        conout( "Erro: " + oWsdl:cError )
        Return aRet
    endif

    // Define o valor de cada parameto necessario
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

//-------------------------------------------------------------------
/*/{Protheus.doc} TDataArqTFS
Função auxiliar, para buscar data e hora de commit de um artefato commitado no TFS
@author jose.camilo
@since 29/07/2017
@function TDataArqTFS
@receiver Caminho completo do arquivo ,Collection e changeset do TFS referente ao artefato
@return data e hora do commit
/*/
User Function TDataArqTFS(cArquivo,cCollection,cChangeSet)

    Local oWsdl
    Local aOps
    Local xRet
    Local aRet := {}
    Local nI := 0
    Local nPos := 0
    Local cResp

    Default cArquivo := ""
    Default cChangeSet := ""
    Default cCollection := ""

    SET DATE FORMAT "dd/mm/yyyy"

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

    // Define a operacao
    xRet := oWsdl:SetOperation( "GetDateCheckin" )

    if xRet == .F.
        conout( "Erro: " + oWsdl:cError )
        Return aRet
    endif

    // Define o valor de cada parameto necessario
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

//-------------------------------------------------------------------
/*/{Protheus.doc} ListaArqChangeSet
Atraves do changeset, lista os artefatos commitados no TFS
@since 29/07/2017
@wsmethod ListaArqChangeSet
@verbo GET
@receiver Collection e changeset do TFS referente aos artefatos
@return logico + mensagem + estrutura com artefatos
/*/
WSRESTFUL ListaArqChangeSet DESCRIPTION "Lista arquivos pelo ChangeSet" FORMAT "application/json"
    WSDATA Collection 		AS STRING OPTIONAL
    WSDATA ChangeSet 		AS STRING OPTIONAL

    WSMETHOD GET  DESCRIPTION "Lista arquivos pelo ChangeSet" 	PRODUCES APPLICATION_JSON
END WSRESTFUL

WSMETHOD GET  WSRECEIVE Collection,ChangeSet WSSERVICE ListaArqChangeSet

    Local nI := 0
    Local aArquivos := {}
    Local lRet  := .T.
    Local cMsg  := "Listagem de arquivos"
    Local oJson := JsonUtil():New()
    Local oItem := Nil
    Local aObj  := {}
    Local aFonte := {}
    Local cFonte := ""

    If Empty(Self:Collection) .Or. Empty(Self:ChangeSet)
        SetRestFault(400, 'Collection ou Changeset dos arquivos que serão verificados não foi informado') 
        Return .F.
    EndIf

    aArquivos := u_TListArqTFS(Self:Collection,Self:ChangeSet)

    If ValType(aArquivos) != "A"
        SetRestFault(400, 'Erro ao buscar itens no TFS')
        Return .F.
    EndIf

    //Retorno dos itens
    for nI:= 1 to Len(aArquivos)

        aFonte := StrTokArr(aArquivos[nI],"/")
        cFonte := aFonte[len(aFonte)]

        if !(upper(right(cFonte,3)) $ 'PRW/PRX')
            loop
        endif

        oItem := JsonUtil():New()
        oItem:PutVal("nome", cFonte)
        oItem:PutVal("path", aArquivos[nI])
        aadd(aObj,oItem)
    next nI

    oJson:PutVal("result", lRet)
    oJson:PutVal("msg", cMsg)
    oJson:PutVal("obj", aObj)

    Self:SetResponse( oJson:ToJson() )  

Return .T.

//-------------------------------------------------------------------
/*/{Protheus.doc} VerificaAlias
Verifica se a tabela existe no ambiente e se possivel a cria
@author jose.camilo
@since 29/07/2017
@wsmethod VerificaAlias
@verbo GET
@receiver nome da tabela
@return logico + mensagem
/*/
WSRESTFUL VerificaAlias DESCRIPTION "Verifica e cria alias" FORMAT "application/json"
    WSDATA Alias 		AS STRING OPTIONAL
    WSDATA Empresa 		AS STRING OPTIONAL

    WSMETHOD GET  DESCRIPTION "Verifica e cria alias" 	PRODUCES APPLICATION_JSON
END WSRESTFUL

WSMETHOD GET  WSRECEIVE Alias,Empresa WSSERVICE VerificaAlias

    Local cError := ""
    Local oError := ErrorBlock({|e| cError := e:Description})
    Local lRet  := .T.
    Local cMsg  := "Tabela existe no ambiente"
    Local oJson := JsonUtil():New()
    Local cBkpEmp       := If(Valtype(cEmpAnt)=="C",cEmpAnt,"")

    If Empty(Self:Alias)
        SetRestFault(400, 'Tabela que será verificada não foi informada') 
        Return .F.
    EndIf

    If !Empty(Self:Empresa)
        RpcClearEnv()
        RpcSetType(3)
        RpcSetEnv(Self:Empresa)
    EndIf
 
    Begin Sequence
        DbSelectArea(Self:Alias)
        DBCloseArea()
    End Sequence

    ErrorBlock(oError)

    If !Empty(cError)
        lRet := .F.
        cMsg := EspecMsg(cError)
    EndIf

    oJson:PutVal("result",lRet)
    oJson:PutVal("msg", cMsg)
    oJson:PutVal("obj",{})

    Self:SetResponse( oJson:ToJson() )

    If !Empty(Self:Empresa) .And. !Empty(cBkpEmp)
        RpcClearEnv()
        RpcSetType(3)
        RpcSetEnv(cBkpEmp)
    EndIf

Return .T.

//-------------------------------------------------------------------
/*/{Protheus.doc} VerificaIndice
Verifica a existencia do indice fisicamente na tabela
@author marllon.fernandes
@since 29/07/2017
@wsmethod VerificaIndice
@verbo GET
@receiver nome da tabela e indice
@return logico + mensagem
/*/
WSRESTFUL VerificaIndice DESCRIPTION "Verifica a existencia do indice fisicamente na tabela" FORMAT "application/json"
    WSDATA alias        AS STRING OPTIONAL
    WSDATA order        AS INTEGER OPTIONAL
    WSDATA nickName     AS STRING OPTIONAL
    WSDATA Empresa 		AS STRING OPTIONAL

    WSMETHOD GET  DESCRIPTION "Verifica a existencia do indice fisicamente na tabela" 	PRODUCES APPLICATION_JSON
END WSRESTFUL

WSMETHOD GET WSRECEIVE alias, order, nickName, Empresa WSSERVICE VerificaIndice

    Local cError := ""
    Local oError := ErrorBlock({|e| cError := e:Description})
    Local oJson  := JsonUtil():New()
    Local cBkpEmp       := If(Valtype(cEmpAnt)=="C",cEmpAnt,"")
    
    If Empty(Self:alias)
        SetRestFault(400, 'Alias que será verificada não foi informado') 
        Return .F.
    EndIf

    If Empty(Self:order) .And. Empty(Self:nickName)
        SetRestFault(400, 'Indice que será verificado não foi informado') 
        Return .F.
    EndIf

    If !Empty(Self:Empresa)
        RpcClearEnv()
        RpcSetType(3)
        RpcSetEnv(Self:Empresa)
    EndIf

    Begin Sequence
        dbSelectArea(self:alias)
        if empty( self:nickName )
            dbSetOrder(self:order)
        else
            dbOrderNickName(self:nickName)
        endif
        dbCloseArea()
    End Sequence

    ErrorBlock(oError)

    If Empty(cError)
        oJson:PutVal("result",.T.)
        oJson:PutVal("msg","Indice existe")
        oJson:PutVal("obj",{})
        Self:SetResponse( oJson:ToJson() )
    Else
        oJson:PutVal("result",.F.)
        oJson:PutVal("msg",EspecMsg(cError))
        oJson:PutVal("obj",{})
        Self:SetResponse( oJson:ToJson() )
    EndIf

    If !Empty(Self:Empresa) .And. !Empty(cBkpEmp)
        RpcClearEnv()
        RpcSetType(3)
        RpcSetEnv(cBkpEmp)
    EndIf

Return .T.

//-------------------------------------------------------------------
/*/{Protheus.doc} dicFileCreate
Cria arquivo no formato informado conforme __LocalDriver
@author marllon.fernandes
@since 29/07/2017
@wsmethod dicFileCreate
@verbo PUT
@receiver
@return file
/*/
//-------------------------------------------------------------------
WSRESTFUL dicFileCreate DESCRIPTION "Cria arquivo no formato informado conforme __LocalDriver" FORMAT "application/json"
    WSDATA driver 	    AS STRING
    WSDATA diretorio 	AS STRING
    WSDATA tipo 	    AS STRING   // sx2,sx3,six,sx6,sx7,sx1,sxb
    WSDATA estrutura    AS ARRAY
    WSDATA Empresa 		AS STRING OPTIONAL

	WSMETHOD PUT DESCRIPTION    "Cria arquivo no formato informado conforme __LocalDriver"	PRODUCES APPLICATION_JSON
END WSRESTFUL

WSMETHOD PUT WSSERVICE dicFileCreate

    Local lRet      := .T.
    Local oJson     := JsonUtil():New()
    Local cBody     := ::GetContent()
    Local cFilter   := ''
    Local cName     := ''
    Local cFile     := ''
    Local ext       := '.dbf'
    Local nX        := 0
    Local oRequest
    Local cBkpEmp       := If(Valtype(cEmpAnt)=="C",cEmpAnt,"")

    if empty(cBody)
        SetRestFault(400, "Parametros obrigatorios nao informados no body!")
        lRet := .F.
    else

        if fWJsonDeserialize(alltrim(cBody),@oRequest)

            If !Empty(oRequest:Empresa)
                RpcClearEnv()
                RpcSetType(3)
                RpcSetEnv(oRequest:Empresa)
            EndIf
                
            if !( upper(alltrim(oRequest:tipo)) $ 'SX1;SX2;SX3;SX6;SX7;SXB;SIX' )
                SetRestFault(400, "Tipo informado invalido!")
                lRet := .F.
            endif
        
            // MONTA O NOME DO ARQUIVO
            cName := 'dicFileCreate_' + oRequest:tipo + '_' + dtos(date()) + strtran(time(),":","")
            
            // SE NAO PREENCHER O DRIVER ASSUMIRA O DEFAULT COMO DBF
            if empty(oRequest:driver)
                oRequest:driver := "DBFCDXADS" //__LocalDriver
            endif

            // REGRA EXTENSAO
            if upper(alltrim(oRequest:tipo)) == "DBFCDXADS"
                ext := '.dbf'
            elseif upper(alltrim(oRequest:tipo)) == "CTREECDX"
                ext := '.dtc'
            endif
            // MONTA O NOME DO ARQUIVO + EXTENSAO
            cFile := cName + ext

            do Case
                case upper( allTrim( oRequest:tipo ) ) == 'SX1'

                    // MONTA O FILTRO PODE PASSAR MAIS DE UMA TABELA
                    for nX := 1 to len(oRequest:estrutura)
                        if nX == len(oRequest:estrutura)
                            cFilter += "X1_GRUPO == '" + oRequest:estrutura[nX] + "' "
                        else
                            cFilter += "X1_GRUPO == '" + oRequest:estrutura[nX] + "' .OR. "
                        endif
                    next 
                    
                    //Aplica o Filtro 
                    dbSelectArea('SX1')
                    SX1->( dbSetFilter({|| &cFilter},cFilter) )
                    SX1->( dbGoTop() )

                    SX1->(__dbCopy((RetFileName(cName)),{},,,,,.F.,oRequest:driver/*"DBFCDXADS"*/))

                case upper( allTrim( oRequest:tipo ) ) == 'SX2'

                    // MONTA O FILTRO PODE PASSAR MAIS DE UMA TABELA
                    for nX := 1 to len(oRequest:estrutura)
                        if nX == len(oRequest:estrutura)
                            cFilter += "X2_CHAVE == '" + PadR(oRequest:estrutura[nX],03,'') + "' "
                        else
                            cFilter += "X2_CHAVE == '" + PadR(oRequest:estrutura[nX],03,'') + "' .OR. "
                        endif
                    next 
                    
                    //Aplica o Filtro 
                    dbSelectArea('SX2')
                    SX2->( dbSetFilter({|| &cFilter},cFilter) )
                    SX2->( dbGoTop() )

                    SX2->(__dbCopy((RetFileName(cName)),{},,,,,.F.,oRequest:driver/*"DBFCDXADS"*/))

                case upper( allTrim( oRequest:tipo ) ) == 'SX3'

                    nPos := aScan(oRequest:estrutura,{|x| '*' $ (x) })

                    if nPos > 0
                        cFilter := "X3_ARQUIVO == '" + left(oRequest:estrutura[nPos],3) + "' " 
                    else
                    
                        // MONTA O FILTRO PODE PASSAR MAIS DE CAMPO
                        for nX := 1 to len(oRequest:estrutura)
                            
                            if nX == len(oRequest:estrutura)
                                cFilter += "X3_CAMPO == '" + PadR(oRequest:estrutura[nX],10,'') + "' "
                            else
                                cFilter += "X3_CAMPO == '" + PadR(oRequest:estrutura[nX],10,'')  + "' .OR. "
                            endif
                        next 
                    endif
                    
                    //Aplica o Filtro 
                    dbSelectArea('SX3')
                    SX3->( dbSetFilter({|| &cFilter},cFilter) )
                    SX3->( dbGoTop() )

                    SX3->(__dbCopy((RetFileName(cName)),{},,,,,.F.,oRequest:driver/*"DBFCDXADS"*/))

                case upper( allTrim( oRequest:tipo ) ) == 'SX6'

                    // MONTA O FILTRO PODE PASSAR MAIS DE PARAMETRO
                    for nX := 1 to len(oRequest:estrutura)
                        if nX == len(oRequest:estrutura)
                            cFilter += "X6_VAR == '" + PadR(oRequest:estrutura[nX],10,'') + "' "
                        else
                            cFilter += "X6_VAR == '" + PadR(oRequest:estrutura[nX],10,'') + "' .OR. "
                        endif
                    next 
                    
                    //Aplica o Filtro 
                    dbSelectArea('SX6')
                    SX6->( dbSetFilter({|| &cFilter},cFilter) )
                    SX6->( dbGoTop() )

                    SX6->(__dbCopy((RetFileName(cName)),{},,,,,.F.,oRequest:driver/*"DBFCDXADS"*/))

                case upper( allTrim( oRequest:tipo ) ) == 'SX7'

                    // MONTA O FILTRO PODE PASSAR MAIS DE GATILHO
                    for nX := 1 to len(oRequest:estrutura)
                        if nX == len(oRequest:estrutura)
                            cFilter += "X7_CAMPO == '" + PadR(oRequest:estrutura[nX],10,'') + "' "
                        else
                            cFilter += "X7_CAMPO == '" + PadR(oRequest:estrutura[nX],10,'') + "' .OR. "
                        endif
                    next 
                    
                    //Aplica o Filtro 
                    dbSelectArea('SX7')
                    SX7->( dbSetFilter({|| &cFilter},cFilter) )
                    SX7->( dbGoTop() )

                    SX7->(__dbCopy((RetFileName(cName)),{},,,,,.F.,oRequest:driver/*"DBFCDXADS"*/))

                case upper( allTrim( oRequest:tipo ) ) == 'SXB'

                    // MONTA O FILTRO PODE PASSAR MAIS DE UMA CONSULTA
                    for nX := 1 to len(oRequest:estrutura)
                        if nX == len(oRequest:estrutura)
                            cFilter += "XB_ALIAS == '" + PadR(oRequest:estrutura[nX],06,'') + "' "
                        else
                            cFilter += "XB_ALIAS == '" + PadR(oRequest:estrutura[nX],06,'') + "' .OR. "
                        endif
                    next 
                    
                    //Aplica o Filtro 
                    dbSelectArea('SXB')
                    SXB->( dbSetFilter({|| &cFilter},cFilter) )
                    SXB->( dbGoTop() )

                    SXB->(__dbCopy((RetFileName(cName)),{},,,,,.F.,oRequest:driver/*"DBFCDXADS"*/))

                case upper( allTrim( oRequest:tipo ) ) == 'SIX'

                    // MONTA O FILTRO PODE PASSAR MAIS DE UM INDICE
                    for nX := 1 to len(oRequest:estrutura)
                        if nX == len(oRequest:estrutura)
                            cFilter += "INDICE == '" + PadR(oRequest:estrutura[nX],03,'') + "' "
                        else
                            cFilter += "INDICE == '" + PadR(oRequest:estrutura[nX],03,'') + "' .OR. "
                        endif
                    next 
                    
                    //Aplica o Filtro
                    dbSelectArea('SIX')
                    SIX->( dbSetFilter({|| &cFilter},cFilter) )
                    SIX->( dbGoTop() )

                    SIX->(__dbCopy((RetFileName(cName)),{},,,,,.F.,oRequest:driver/*"DBFCDXADS"*/))
                
            endCase
            
            if file( GetSrvProfString("Startpath","") + cFile )
                        
                if right(oRequest:diretorio,1) != '/'
                    oRequest:diretorio := oRequest:diretorio + '/'
                endif

                if !( __CopyFile( GetSrvProfString("Startpath","") + cFile , oRequest:diretorio + cFile ) )
                    SetRestFault(400, 'erro ao copiar arquivo para: ' + oRequest:diretorio)
                    lRet := .F.
                else                        
                    oJson:PutVal("result",.T.)
                    oJson:PutVal("msg",'arquivo copiado com sucesso para: ' + oRequest:diretorio + cFile)
                    oJson:PutVal("arquivo", cFile)
                    oJson:PutVal("diretorio", oRequest:diretorio)
                    oJson:PutVal("obj",{})
                    oJson:PutVal("server",getServerIP())
                    FErase( GetSrvProfString("Startpath","") + cFile ) 
                    ::SetResponse( oJson:ToJson() )
                endif
            else
                SetRestFault(400, 'nao foi possivel gerar arquivo: ' + cFile)
                lRet := .F.
            endif        

        
        else
            SetRestFault(400, "Nao foi possivel realizar o parser!")
            lRet := .F.
        endif
        
    endif

    If !Empty(Self:Empresa) .And. !Empty(cBkpEmp)
        RpcClearEnv()
        RpcSetType(3)
        RpcSetEnv(cBkpEmp)
    EndIf

return lRet


//-------------------------------------------------------------------
/*/{Protheus.doc} EstruturaSxs
Retorna a estrutura de um dicionario
@author jose.camilo
@since 29/07/2017
@wsmethod EstruturaSxs
@verbo GET
@receiver
@return logico + mensagem + estrutura solicitada
/*/
WSRESTFUL EstruturaSxs DESCRIPTION "Retorna a estrutura de um dicionario" FORMAT "application/json"
    WSDATA Tipo 		AS STRING OPTIONAL
    WSDATA Valor 		AS STRING OPTIONAL
    WSDATA Filial 		AS STRING OPTIONAL 
    WSDATA Ordem 		AS STRING OPTIONAL
    WSDATA Sequencia	AS STRING OPTIONAL
    WSDATA Empresa 		AS STRING OPTIONAL

    WSMETHOD GET  DESCRIPTION "Retorna a estrutura de um dicionario" 	PRODUCES APPLICATION_JSON
END WSRESTFUL

WSMETHOD GET  WSRECEIVE Tipo, Valor, Filial, Ordem, Sequencia, Empresa WSSERVICE EstruturaSxs

    Local cBusca        := ""
    Local nTamSXs       := 0
    Local nI            := 0
    Local cTitulo
    Local xConteudo
    Local nOrder        := 1
    Local cDicionario   := ""
    Local cFilSelect := If(Empty(self:Filial) , Space(Len(cFilAnt)) , self:Filial )
    Local cIndice       := ""
    Local lRet  := .T.
    Local cMsg  := "Estrutura encontrada"
    Local oJson := JsonUtil():New()
    Local oItem := Nil
    Local aObj  := {}
    Local cBkpEmp       := If(Valtype(cEmpAnt)=="C",cEmpAnt,"")

    If !Empty(Self:Empresa)
        RpcClearEnv()
        RpcSetType(3)
        RpcSetEnv(Self:Empresa)
    EndIf

    If Upper(self:Tipo) == 'TABELA'

        cDicionario := 'SX2'
        nOrder  := 1
        cIndice := 'X2_CHAVE'
        cBusca  := readValue(cDicionario, nOrder, PadR(Upper(self:Valor),3) , cIndice)

        If Empty(cBusca)
            SetRestFault(400, "Tabela não encontrado no dicionário de dados")
            Return .F.
        EndIf

    ElseIf Upper(self:Tipo) == 'CAMPO'

        cDicionario := 'SX3'
        nOrder  := 2
        cIndice := 'X3_CAMPO'
        cBusca  := readValue(cDicionario, nOrder, PadR(Upper(self:Valor),10) , cIndice)

        If Empty(cBusca)
            SetRestFault(400, "Campo não encontrado no dicionário de dados")
            Return .F.
        EndIf

    ElseIf Upper(self:Tipo) == 'PARAMETRO'

        cDicionario := 'SX6'
        nOrder  := 1
        cIndice := 'X6_FIL+X6_VAR'
        cBusca  := readValue(cDicionario, nOrder, cFilSelect + PadR(Upper(self:Valor),10) , cIndice)

        If Empty(cBusca)
            SetRestFault(400, "Parametro não encontrado no dicionário de dados")
            Return .F.
        EndIf

    ElseIf Upper(self:Tipo) == 'GATILHO'

        cDicionario := 'SX7'
        nOrder  := 1
        cIndice := 'X7_CAMPO+X7_SEQUENC'
        cBusca  := readValue(cDicionario, nOrder, PadR(Upper(self:Valor),10) + self:Sequencia, cIndice)

        If Empty(cBusca)
            SetRestFault(400, "Gatilho não encontrado no dicionário de dados")        
            Return .F.
        EndIf

    ElseIf Upper(self:Tipo) == 'INDICE'

        cDicionario := 'SIX'
        nOrder  := 1
        cIndice := 'INDICE+ORDEM'
        cBusca  := readValue(cDicionario, nOrder, PadR(Upper(self:Valor),3) + self:Ordem , cIndice)

        If Empty(cBusca)
            SetRestFault(400, "Indice não encontrado no dicionário de dados")                
            Return .F.
        EndIf

    ElseIf Upper(self:Tipo) == 'CONSULTA'

        cDicionario := 'SXB'
        nOrder  := 1
        cIndice := 'XB_ALIAS'
        cBusca  := readValue(cDicionario, nOrder, PadR(Upper(self:Valor),6) , cIndice)

        If Empty(cBusca)
            SetRestFault(400, "Consulta não encontrado no dicionário de dados")                        
            Return .F.
        EndIf

    EndIf

    dbSelectArea(cDicionario)
    dbSetOrder(nOrder)
    dbSeek(cBusca)

    nTamSXs := FCOUNT()

    //Retorno dos itens
    While &(cDicionario)->( &(cIndice)) == cBusca

        oItem := JsonUtil():New()

        For nI:=1 to nTamSXs
            cTitulo := FIELD(nI)
            xConteudo := &(cDicionario)->( &(cTitulo) )

            if ValType(xConteudo) == 'C'

                xConteudo := AllTrim( StrTran(xConteudo, '"', "'") )

                If cTitulo $ "X3_USADO;X3_RESERV"
                    oItem:PutVal(cTitulo, Encode64( xConteudo ) )
                Else
                    oItem:PutVal(cTitulo, xConteudo )                
                EndIf

            ElseIf ValType(xConteudo) == 'N'
                oItem:PutVal(cTitulo, cValToChar(xConteudo) )

            ElseIf ValType(xConteudo) == 'L'
                oItem:PutVal(cTitulo, xConteudo )

            Endif
        Next nI
        
        aadd(aObj,oItem)
        dbSkip()
    EndDo

    DBCloseArea()
    
    oJson:PutVal("result",lRet)
    oJson:PutVal("msg", cMsg)
    oJson:PutVal("obj",aObj)

    Self:SetResponse( oJson:ToJson() )

    If !Empty(Self:Empresa) .And. !Empty(cBkpEmp)
        RpcClearEnv()
        RpcSetType(3)
        RpcSetEnv(cBkpEmp)
    EndIf

Return .T.


//-------------------------------------------------------------------
/*/{Protheus.doc} NomeCampos
Retorna o nome dos campos de uma tabela
@author jose.camilo
@since 29/07/2017
@wsmethod NomeCampos
@verbo GET
@receiver tabela
@return logico + mensagem + campos
/*/
WSRESTFUL NomeCampos DESCRIPTION "Retorna o nome dos campos de uma tabela" FORMAT "application/json"
    WSDATA Tabela	AS STRING
    WSDATA Empresa 		AS STRING OPTIONAL

    WSMETHOD GET  DESCRIPTION "Retorna o nome dos campos de uma tabela" 	PRODUCES APPLICATION_JSON
END WSRESTFUL

WSMETHOD GET  WSRECEIVE Tabela, Empresa WSSERVICE NomeCampos

    Local nTamanho  := 0
    Local nI        := 0
    Local aCampos   := {}
    Local oJson     := JsonUtil():New()
    Local cBkpEmp       := If(Valtype(cEmpAnt)=="C",cEmpAnt,"")

    If Empty(Self:Tabela)
        SetRestFault(400, "Tabela não informada")        
        Return .F.
    EndIf

    If !Empty(Self:Empresa)
        RpcClearEnv()
        RpcSetType(3)
        RpcSetEnv(Self:Empresa)
    EndIf

    Begin Sequence
        dbSelectArea(Self:Tabela)
    End Sequence

    ErrorBlock(oError)

    If !Empty(cError)
        lRet := .F.
        cMsg := EspecMsg(cError)
    Else
    
        nTamanho := FCOUNT()

        For nI:=1 to nTamanho
            aAdd(aCampos , FIELD(nI) ) 
        Next nI

        DBCloseArea()
    EndIf
    

    oJson:PutVal("result",.T.)
    oJson:PutVal("msg","Consulta realizada")
    oJson:PutVal("obj",aCampos)

    Self:SetResponse( oJson:ToJson() )    

    If !Empty(Self:Empresa) .And. !Empty(cBkpEmp)
        RpcClearEnv()
        RpcSetType(3)
        RpcSetEnv(cBkpEmp)
    EndIf

Return .T.

//-------------------------------------------------------------------
/*/{Protheus.doc} VerificaAtributoCampo
Verifica se o conteudo de um atributo de um campo, esta atualizado no ambiente
@author jose.camilo
@since 29/07/2017
@wsmethod VerificaAtributoCampo
@verbo GET
@receiver campo, atributo e valor esperado
@return logico + mensagem
/*/
WSRESTFUL VerificaAtributoCampo DESCRIPTION "Retorna se o conteudo de um atributo de um campo, esta correto" FORMAT "application/json"
    WSDATA Campo	AS STRING
    WSDATA Atributo	AS STRING
    WSDATA Valor	AS STRING
    WSDATA Empresa	AS STRING OPTIONAL

    WSMETHOD GET  DESCRIPTION "Retorna se o conteudo de um atributo de um campo, esta correto" 	PRODUCES APPLICATION_JSON
END WSRESTFUL

WSMETHOD GET  WSRECEIVE Campo, Atributo, Valor, Empresa WSSERVICE VerificaAtributoCampo

    Local oJson     := JsonUtil():New()
    Local xValorAmb
    Local lRet := .F.
    Local cMsg := "Atributo de campo desatualizado"
    Local cBkpEmp       := If(Valtype(cEmpAnt)=="C",cEmpAnt,"")

    If Empty(Self:Campo) .Or. Empty(Self:Atributo) .Or. Empty(Self:Valor)
        SetRestFault(400, 'Campo, Atributo ou Valor não informado') 
        Return .F.
    EndIf

    If !Empty(Self:Empresa)
        RpcClearEnv()
        RpcSetType(3)
        RpcSetEnv(Self:Empresa)
    EndIf

    xValorAmb   := readValue('SX3', 2, PadR(Upper(Self:Campo),10) , Upper(Self:Atributo))

    If AllTrim( cValToChar(xValorAmb) ) == Self:Valor
        lRet := .T.
        cMsg := "Atributo de campo atualizado"
    EndIf

    DBCloseArea()

    oJson:PutVal("result",lRet)
    oJson:PutVal("msg",cMsg)
    oJson:PutVal("obj", {})

    Self:SetResponse( oJson:ToJson() )   

    If !Empty(Self:Empresa) .And. !Empty(cBkpEmp)
        RpcClearEnv()
        RpcSetType(3)
        RpcSetEnv(cBkpEmp)
    EndIf

Return .T.

//-------------------------------------------------------------------
/*/{Protheus.doc} VerificaConsulta
Verifica se uma consulta padrao sxb, existe no ambiente
@author jose.camilo
@since 29/07/2017
@wsmethod VerificaConsulta
@verbo GET
@receiver consulta
@return logico + mensagem
/*/
WSRESTFUL VerificaConsulta DESCRIPTION "Retorna se uma consulta existe" FORMAT "application/json"
    WSDATA Consulta	AS STRING
    WSDATA Empresa	AS STRING OPTIONAL

    WSMETHOD GET  DESCRIPTION "Retorna se uma consulta existe" 	PRODUCES APPLICATION_JSON
END WSRESTFUL

WSMETHOD GET  WSRECEIVE Consulta, Empresa WSSERVICE VerificaConsulta

    Local oJson     := JsonUtil():New()
    Local lRet := .F.
    Local cMsg := "Consulta não existe"
    Local cBkpEmp       := If(Valtype(cEmpAnt)=="C",cEmpAnt,"")

    If Empty(Self:Consulta)
        SetRestFault(400, 'Consulta não informada') 
        Return .F.
    EndIf

    If !Empty(Self:Empresa)
        RpcClearEnv()
        RpcSetType(3)
        RpcSetEnv(Self:Empresa)
    EndIf

    dbSelectArea('SXB')
    dbSetOrder(1)

    IF dbSeek( PadR(Upper(self:Consulta),6) )
        lRet := .T.
        cMsg := "Consulta existe"
    EndIf

    DBCloseArea()

    oJson:PutVal("result",lRet)
    oJson:PutVal("msg",cMsg)
    oJson:PutVal("obj", {})

    Self:SetResponse( oJson:ToJson() )    

    If !Empty(Self:Empresa) .And. !Empty(cBkpEmp)
        RpcClearEnv()
        RpcSetType(3)
        RpcSetEnv(cBkpEmp)
    EndIf

Return .T.

//-------------------------------------------------------------------
/*/{Protheus.doc} VerificaGatilho
Verifica se um gatilho sx7, existe no ambiente
@author jose.camilo
@since 29/07/2017
@wsmethod VerificaGatilho
@verbo GET
@receiver gatilho e sequencia
@return logico + mensagem
/*/
WSRESTFUL VerificaGatilho DESCRIPTION "Retorna se um Gatilho existe" FORMAT "application/json"
    WSDATA Gatilho	AS STRING
    WSDATA Sequencia	AS STRING
    WSDATA Empresa	AS STRING OPTIONAL

    WSMETHOD GET  DESCRIPTION "Retorna se um Gatilho existe" 	PRODUCES APPLICATION_JSON
END WSRESTFUL

WSMETHOD GET  WSRECEIVE Gatilho, Sequencia, Empresa WSSERVICE VerificaGatilho

    Local oJson := JsonUtil():New()
    Local lRet := .F.
    Local cMsg := "Gatilho não existe"
    Local cBkpEmp       := If(Valtype(cEmpAnt)=="C",cEmpAnt,"")

    If Empty(Self:Gatilho) .Or. Empty(Self:Sequencia)
        SetRestFault(400, 'Gatilho ou Sequencia não informado') 
        Return .F.
    EndIf

    If !Empty(Self:Empresa)
        RpcClearEnv()
        RpcSetType(3)
        RpcSetEnv(Self:Empresa)
    EndIf

    dbSelectArea('SX7')
    dbSetOrder(1)

    IF dbSeek( PadR(Upper(self:Gatilho),10) + self:Sequencia)
        lRet := .T.
        cMsg := "Gatilho existe"
    EndIf

    DBCloseArea()

    oJson:PutVal("result",lRet)
    oJson:PutVal("msg",cMsg)
    oJson:PutVal("obj", {})

    Self:SetResponse( oJson:ToJson() )    

    If !Empty(Self:Empresa) .And. !Empty(cBkpEmp)
        RpcClearEnv()
        RpcSetType(3)
        RpcSetEnv(cBkpEmp)
    EndIf

Return .T.


//-------------------------------------------------------------------
/*/{Protheus.doc} GravaCampo
Recupera um campo de uma base e grava no ambiente corrente
@author jose.camilo
@since 29/07/2017
@wsmethod GravaCampo
@verbo POST
@receiver campo e base de origem
@return logico + mensagem
/*/
WSRESTFUL GravaCampo DESCRIPTION "Recupera o dado de uma base e grava na corrente" FORMAT "application/json"
    WSDATA Origem	AS STRING
    WSDATA Campo	AS STRING
    WSDATA Empresa	AS STRING OPTIONAL

    WSMETHOD POST  DESCRIPTION "Recupera o dado de uma base e grava na corrente" 	PRODUCES APPLICATION_JSON
END WSRESTFUL

WSMETHOD POST WSSERVICE GravaCampo

    Local lRet      := .T.
    Local oJson     := JsonUtil():New()
    Local cBody     := Self:GetContent()
    Local nX        := 0
    Local nHeader  := 0
    Local oRequest
    Local oRestClient
    Local oReqAux
    Local xConteudo
    Local cBkpEmp       := If(Valtype(cEmpAnt)=="C",cEmpAnt,"")

    if empty(cBody)
        SetRestFault(400, "Parametros obrigatorios nao informados no body!")
        lRet := .F.
    else

        if fWJsonDeserialize(alltrim(cBody),@oRequest)

            If !Empty(oRequest:Empresa)
                RpcClearEnv()
                RpcSetType(3)
                RpcSetEnv(oRequest:Empresa)
            EndIf

            oRestClient := FWRest():New(oRequest:Origem)
            oRestClient:setPath("/EstruturaSxs?tipo=campo&valor=" + oRequest:Campo + "&empresa=" + cEmpAnt)
            If oRestClient:Get()

                if fWJsonDeserialize(oRestClient:GetResult(),@oReqAux)

                    If oReqAux:Result
                    
                        dbSelectArea('SX3')
                        nHeader := FCOUNT()

                        RecLock('SX3', .T.)

                            For nX:=1 to nHeader
                                cTitulo := FIELD(nX)
                                xConteudo := &('oReqAux:estrutura[1]:'+ cTitulo)

                                If cTitulo $ "X3_USADO;X3_RESERV"
                                    SX3->( &(cTitulo) ) := Decode64( xConteudo )
                                Else                
                                    SX3->( &(cTitulo) ) := xConteudo
                                EndIf

                            Next nX

                        MsUnlock()

                        DBCloseArea()

                        oJson:PutVal("result",lRet)
                        oJson:PutVal("msg","Copia realizada")
                        oJson:PutVal("obj", {})

                        Self:SetResponse( oJson:ToJson() )  
                    Else
                        SetRestFault(400, "Erro na Origem: " + oReqAux:Msg)
                        lRet := .F.
                    endif
                else
                    SetRestFault(400, "Nao foi possivel realizar o parser do EstruturaSxs!")
                    lRet := .F.
                EndIf

                
            Else
                conout(oRestClient:GetLastError())
                SetRestFault(400, "Nao foi possivel buscar dados na origem!")
                lRet := .F.
            Endif

        else
            SetRestFault(400, "Nao foi possivel realizar o parser do GravaCampo!")
            lRet := .F.
        EndIf

    EndIf

    If !Empty(Self:Empresa) .And. !Empty(cBkpEmp)
        RpcClearEnv()
        RpcSetType(3)
        RpcSetEnv(cBkpEmp)
    EndIf

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} GravaAtributoCampo
Recupera o atributo de um campo em uma base e grava na corrente
@author jose.camilo
@since 29/07/2017
@wsmethod GravaAtributoCampo
@verbo PUT
@receiver campo, atributo e base de origem
@return logico + mensagem
/*/
WSRESTFUL GravaAtributoCampo DESCRIPTION "Recupera o atributo de um campo em uma base e grava na corrente" FORMAT "application/json"
    WSDATA Origem	AS STRING
    WSDATA Campo	AS STRING
    WSDATA Atributo	AS STRING
    WSDATA Empresa	AS STRING OPTIONAL

    WSMETHOD PUT  DESCRIPTION "Recupera o atributo de um campo em uma base e grava na corrente" 	PRODUCES APPLICATION_JSON
END WSRESTFUL

WSMETHOD PUT WSSERVICE GravaAtributoCampo

    Local lRet      := .T.
    Local oJson     := JsonUtil():New()
    Local cBody     := Self:GetContent()
    Local oRequest
    Local cAtributo
    Local cOrigem
    Local cCampo
    Local oRestClient
    Local oReqAux
    Local xConteudo
    Local cBkpEmp       := If(Valtype(cEmpAnt)=="C",cEmpAnt,"")

    if empty(cBody)
        SetRestFault(400, "Parametros obrigatorios nao informados no body!")
        lRet := .F.
    else

        if fWJsonDeserialize(alltrim(cBody),@oRequest)

            If !Empty(oRequest:Empresa)
                RpcClearEnv()
                RpcSetType(3)
                RpcSetEnv(oRequest:Empresa)
            EndIf

            cOrigem := oRequest:Origem
            cCampo := oRequest:Campo
            cAtributo := oRequest:Atributo

            oRestClient := FWRest():New(cOrigem)
            oRestClient:setPath("/EstruturaSxs?tipo=campo&valor=" + cCampo + "&empresa=" + cEmpAnt)
            
            If oRestClient:Get()

                if fWJsonDeserialize(oRestClient:GetResult(),@oReqAux)

                    If oReqAux:Result
                    
                        dbSelectArea('SX3')
                        dbSetOrder(2)
                        If dbSeek(PadR(Upper(cCampo),10))
                            RecLock('SX3', .F.)

                                cTitulo := cAtributo
                                xConteudo := &('oReqAux:estrutura[1]:'+ cTitulo)

                                If cTitulo $ "X3_USADO;X3_RESERV"
                                    SX3->( &(cTitulo) ) := Decode64( xConteudo )
                                Else                
                                    SX3->( &(cTitulo) ) := xConteudo
                                EndIf

                            MsUnlock()
                        else
                            SetRestFault(400, "Campo não encontrado no Destino")
                            lRet := .F.
                        EndIf

                        DBCloseArea()

                        oJson:PutVal("result",lRet)
                        oJson:PutVal("msg","Copia realizada")
                        oJson:PutVal("obj", {})
                        
                        Self:SetResponse( oJson:ToJson() )  
                    Else
                        SetRestFault(400, "Erro na Origem: " + oReqAux:Msg)
                        lRet := .F.
                    endif
                else
                    SetRestFault(400, "Nao foi possivel realizar o parser do EstruturaSxs!")
                    lRet := .F.
                EndIf

                
            Else
                conout(oRestClient:GetLastError())
                SetRestFault(400, "Nao foi possivel buscar dados na origem!")
                lRet := .F.
            Endif

        else
            SetRestFault(400, "Nao foi possivel realizar o parser do GravaCampo!")
            lRet := .F.
        EndIf

    EndIf

    If !Empty(Self:Empresa) .And. !Empty(cBkpEmp)
        RpcClearEnv()
        RpcSetType(3)
        RpcSetEnv(cBkpEmp)
    EndIf

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} GravaParametro
Recupera um parametro em uma base de origem e grava no ambiente corrente
@author jose.camilo
@since 29/07/2017
@wsmethod GravaParametro
@verbo POST
@receiver parametro, filial e ambiente origem
@return logico + mensagem
/*/
WSRESTFUL GravaParametro DESCRIPTION "Recupera um parametro em uma base de origem e grava no ambiente corrente" FORMAT "application/json"
    WSDATA Origem	AS STRING
    WSDATA Parametro	AS STRING
    WSDATA Filial	AS STRING
    WSDATA Empresa	AS STRING OPTIONAL

    WSMETHOD POST  DESCRIPTION "Recupera um parametro em uma base de origem e grava no ambiente corrente" 	PRODUCES APPLICATION_JSON
END WSRESTFUL

WSMETHOD POST WSSERVICE GravaParametro

    Local lRet      := .T.
    Local oJson     := JsonUtil():New()
    Local cBody     := Self:GetContent()
    Local nX        := 0
    Local nHeader  := 0
    Local oRequest
    Local oRestClient
    Local oReqAux
    Local xConteudo
    Local cBkpEmp       := If(Valtype(cEmpAnt)=="C",cEmpAnt,"")

    if empty(cBody)
        SetRestFault(400, "Parametros obrigatorios nao informados no body!")
        lRet := .F.
    else

        if fWJsonDeserialize(alltrim(cBody),@oRequest)

            cFilSelect := If(Empty(oRequest:Filial) , Space(Len(cFilAnt)) ,oRequest:Filial )
            cParametro := oRequest:Parametro
            
            If !Empty(oRequest:Empresa)
                RpcClearEnv()
                RpcSetType(3)
                RpcSetEnv(oRequest:Empresa)
            EndIf

            oRestClient := FWRest():New(oRequest:Origem)
            oRestClient:setPath("/EstruturaSxs?tipo=parametro&valor=" + oRequest:Parametro + "&filial="+ oRequest:Filial + "&empresa=" + cEmpAnt)
            If oRestClient:Get()

                if fWJsonDeserialize(oRestClient:GetResult(),@oReqAux)

                    If oReqAux:Result
                    
                        dbSelectArea('SX6')
                        nHeader := FCOUNT()

                        dbSetOrder(1)
                        
                        If dbSeek( cFilSelect + PadR(Upper(cParametro),10) )
                            
                            RecLock('SX6', .F.)

                                SX6->X6_CONTEUD := &('oReqAux:estrutura[1]:'+ X6_CONTEUD)
                                SX6->X6_CONTSPA := &('oReqAux:estrutura[1]:'+ X6_CONTSPA)
                                SX6->X6_CONTENG := &('oReqAux:estrutura[1]:'+ X6_CONTENG)

                            MsUnlock()

                        else

                            RecLock('SX6', .T.)

                                For nX:=1 to nHeader
                                    cTitulo := FIELD(nX)
                                    xConteudo := &('oReqAux:estrutura[1]:'+ cTitulo)

                                    SX6->( &(cTitulo) ) := xConteudo

                                Next nX

                            MsUnlock()
                        EndIf

                        DBCloseArea()

                        oJson:PutVal("result",lRet)
                        oJson:PutVal("msg","Copia realizada")
                        oJson:PutVal("obj", {})
                        
                        Self:SetResponse( oJson:ToJson() )  
                    Else
                        SetRestFault(400, "Erro na Origem: " + oReqAux:Msg)
                        lRet := .F.
                    endif
                else
                    SetRestFault(400, "Nao foi possivel realizar o parser do EstruturaSxs!")
                    lRet := .F.
                EndIf

            Else
                conout(oRestClient:GetLastError())
                SetRestFault(400, "Nao foi possivel buscar dados na origem!")
                lRet := .F.
            Endif

        else
            SetRestFault(400, "Nao foi possivel realizar o parser do GravaCampo!")
            lRet := .F.
        EndIf

    EndIf

    If !Empty(Self:Empresa) .And. !Empty(cBkpEmp)
        RpcClearEnv()
        RpcSetType(3)
        RpcSetEnv(cBkpEmp)
    EndIf

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} GravaAtributoParametro
Recupera um atributo de um parametro em um ambiente de origem e grava no corrente
@author jose.camilo
@since 29/07/2017
@wsmethod GravaAtributoParametro
@verbo PUT
@receiver parametro, filial do parametro, atributo e ambiente de origem
@return logico + mensagem
/*/
WSRESTFUL GravaAtributoParametro DESCRIPTION "Recupera um atributo de um parametro em um ambiente de origem e grava no corrente" FORMAT "application/json"
    WSDATA Origem	AS STRING
    WSDATA Parametro	AS STRING
    WSDATA Filial	AS STRING
    WSDATA Atributo	AS STRING
    WSDATA Empresa 		AS STRING OPTIONAL

    WSMETHOD PUT  DESCRIPTION "Recupera um atributo de um parametro em um ambiente de origem e grava no corrente" 	PRODUCES APPLICATION_JSON
END WSRESTFUL

WSMETHOD PUT WSSERVICE GravaAtributoParametro

    Local lRet      := .T.
    Local oJson     := JsonUtil():New()
    Local cBody     := Self:GetContent()
    Local oRequest
    Local cAtributo
    Local cOrigem
    Local cFilSelect
    Local cParametro
    Local oRestClient
    Local oReqAux
    Local xConteudo
    Local cBkpEmp       := If(Valtype(cEmpAnt)=="C",cEmpAnt,"")


    if empty(cBody)
        SetRestFault(400, "Parametros obrigatorios nao informados no body!")
        lRet := .F.
    else

        if fWJsonDeserialize(alltrim(cBody),@oRequest)

            cOrigem := oRequest:Origem
            cFilSelect := If(Empty(oRequest:Filial) , Space(Len(cFilAnt)) ,oRequest:Filial )
            cParametro := oRequest:Parametro
            cAtributo := oRequest:Atributo
            
            If !Empty(oRequest:Empresa)
                RpcClearEnv()
                RpcSetType(3)
                RpcSetEnv(oRequest:Empresa)
            EndIf

            oRestClient := FWRest():New(cOrigem)
            oRestClient:setPath("/EstruturaSxs?tipo=parametro&valor=" + oRequest:Parametro + "&filial="+ oRequest:Filial + "&empresa=" + cEmpAnt)
            

            If oRestClient:Get()

                if fWJsonDeserialize(oRestClient:GetResult(),@oReqAux)

                    If oReqAux:Result
                    
                        dbSelectArea('SX6')
                        dbSetOrder(1)
                            
                        If dbSeek( cFilSelect + PadR(Upper(cParametro),10) )
                            RecLock('SX6', .F.)

                                cTitulo := cAtributo
                                xConteudo := &('oReqAux:estrutura[1]:'+ cTitulo)
            
                                SX6->( &(cTitulo) ) := xConteudo

                            MsUnlock()
                        else
                            SetRestFault(400, "Parametro não encontrado no Destino")
                            lRet := .F.
                        EndIf

                        DBCloseArea()

                        oJson:PutVal("result",lRet)
                        oJson:PutVal("msg","Copia realizada")
                        oJson:PutVal("obj", {})
                        
                        Self:SetResponse( oJson:ToJson() )  
                    Else
                        SetRestFault(400, "Erro na Origem: " + oReqAux:Msg)
                        lRet := .F.
                    endif
                else
                    SetRestFault(400, "Nao foi possivel realizar o parser do EstruturaSxs!")
                    lRet := .F.
                EndIf

            Else
                conout(oRestClient:GetLastError())
                SetRestFault(400, "Nao foi possivel buscar dados na origem!")
                lRet := .F.
            Endif

        else
            SetRestFault(400, "Nao foi possivel realizar o parser do GravaCampo!")
            lRet := .F.
        EndIf

    EndIf

    If !Empty(Self:Empresa) .And. !Empty(cBkpEmp)
        RpcClearEnv()
        RpcSetType(3)
        RpcSetEnv(cBkpEmp)
    EndIf

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} GravaGatilho
Recupera um gatilho de uma base de rigem e grava na corrente
@author jose.camilo
@since 29/07/2017
@wsmethod GravaGatilho
@verbo POST
@receiver gatilho, sequencia do gatilho e ambiente de origem
@return logico + mensagem
/*/
WSRESTFUL GravaGatilho DESCRIPTION "Recupera um gatilho de uma base de rigem e grava na corrente" FORMAT "application/json"
    WSDATA Origem	AS STRING
    WSDATA Gatilho	AS STRING
    WSDATA Sequencia AS STRING
    WSDATA Empresa 		AS STRING OPTIONAL

    WSMETHOD POST  DESCRIPTION "Recupera um gatilho de uma base de rigem e grava na corrente" 	PRODUCES APPLICATION_JSON
END WSRESTFUL

WSMETHOD POST WSSERVICE GravaGatilho

    Local lRet      := .T.
    Local oJson     := JsonUtil():New()
    Local cBody     := Self:GetContent()
    Local nX        := 0
    Local nHeader  := 0
    Local oRequest
    Local oRestClient
    Local oReqAux
    Local xConteudo
    Local cBkpEmp       := If(Valtype(cEmpAnt)=="C",cEmpAnt,"")


    if empty(cBody)
        SetRestFault(400, "Parametros obrigatorios nao informados no body!")
        lRet := .F.
    else

        if fWJsonDeserialize(alltrim(cBody),@oRequest)

            If !Empty(oRequest:Empresa)
                RpcClearEnv()
                RpcSetType(3)
                RpcSetEnv(oRequest:Empresa)
            EndIf

            oRestClient := FWRest():New(oRequest:Origem)
            oRestClient:setPath("/EstruturaSxs?tipo=gatilho&valor=" + oRequest:Gatilho + "&sequencia="+ oRequest:Sequencia + "&empresa=" + cEmpAnt)
            If oRestClient:Get()

                if fWJsonDeserialize(oRestClient:GetResult(),@oReqAux)

                    If oReqAux:Result

                        dbSelectArea('SX7')
                        nHeader := FCOUNT()

                        RecLock('SX7', .T.)

                            For nX:=1 to nHeader
                                cTitulo := FIELD(nX)
                                xConteudo := &('oReqAux:estrutura[1]:'+ cTitulo)

                                SX7->( &(cTitulo) ) := xConteudo

                            Next nX

                        MsUnlock()

                        DBCloseArea()

                        oJson:PutVal("result",lRet)
                        oJson:PutVal("msg","Copia realizada")
                        oJson:PutVal("obj", {})
                        
                        Self:SetResponse( oJson:ToJson() )  
                    Else
                        SetRestFault(400, "Erro na Origem: " + oReqAux:Msg)
                        lRet := .F.
                    endif
                else
                    SetRestFault(400, "Nao foi possivel realizar o parser do EstruturaSxs!")
                    lRet := .F.
                EndIf
 
            Else
                conout(oRestClient:GetLastError())
                SetRestFault(400, "Nao foi possivel buscar dados na origem!")
                lRet := .F.
            Endif

        else
            SetRestFault(400, "Nao foi possivel realizar o parser do GravaCampo!")
            lRet := .F.
        EndIf

    EndIf

    If !Empty(Self:Empresa) .And. !Empty(cBkpEmp)
        RpcClearEnv()
        RpcSetType(3)
        RpcSetEnv(cBkpEmp)
    EndIf

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} GravaConsulta
Recupera uma consulta padrao sxb de um ambiente de origem e grava no corrente
@author jose.camilo
@since 29/07/2017
@wsmethod GravaConsulta
@verbo POST
@receiver consulta padrao e ambiente de origem
@return logico + mensagem
/*/
WSRESTFUL GravaConsulta DESCRIPTION "Recupera uma consulta padrao sxb de um ambiente de origem e grava no corrente" FORMAT "application/json"
    WSDATA Origem	AS STRING
    WSDATA Consulta	AS STRING
    WSDATA Empresa 		AS STRING OPTIONAL

    WSMETHOD POST  DESCRIPTION "Recupera uma consulta padrao sxb de um ambiente de origem e grava no corrente" 	PRODUCES APPLICATION_JSON
END WSRESTFUL

WSMETHOD POST WSSERVICE GravaConsulta

    Local lRet      := .T.
    Local oJson     := JsonUtil():New()
    Local cBody     := Self:GetContent()
    Local nX        := 0
    Local nI        := 0
    Local nHeader  := 0
    Local oRequest
    Local oRestClient
    Local oReqAux
    Local xConteudo
    Local cBkpEmp       := If(Valtype(cEmpAnt)=="C",cEmpAnt,"")


    if empty(cBody)
        SetRestFault(400, "Parametros obrigatorios nao informados no body!")
        lRet := .F.
    else

        if fWJsonDeserialize(alltrim(cBody),@oRequest)

            If !Empty(oRequest:Empresa)
                RpcClearEnv()
                RpcSetType(3)
                RpcSetEnv(oRequest:Empresa)
            EndIf

            oRestClient := FWRest():New(oRequest:Origem)
            oRestClient:setPath("/EstruturaSxs?tipo=consulta&valor=" + oRequest:Consulta + "&empresa=" + cEmpAnt)
            If oRestClient:Get()

                if fWJsonDeserialize(oRestClient:GetResult(),@oReqAux)

                    If oReqAux:Result

                        dbSelectArea('SXB')
                        nHeader := FCOUNT()
                    
                        For nI:=1 to Len(oReqAux:estrutura)

                            RecLock('SXB', .T.)
                                For nX:=1 to nHeader
                                    cTitulo := FIELD(nX)
                                    xConteudo := &('oReqAux:estrutura[' + cValToChar(nI) + ']:'+ cTitulo)

                                    SXB->( &(cTitulo) ) := xConteudo

                                Next nX
                            MsUnlock()

                        Next nI

                        DBCloseArea()

                        oJson:PutVal("result",lRet)
                        oJson:PutVal("msg","Copia realizada")
                        oJson:PutVal("obj", {})

                        Self:SetResponse( oJson:ToJson() )  
                    Else
                        SetRestFault(400, "Erro na Origem: " + oReqAux:Msg)
                        lRet := .F.
                    endif
                else
                    SetRestFault(400, "Nao foi possivel realizar o parser do EstruturaSxs!")
                    lRet := .F.
                EndIf

            Else
                conout(oRestClient:GetLastError())
                SetRestFault(400, "Nao foi possivel buscar dados na origem!")
                lRet := .F.
            Endif

        else
            SetRestFault(400, "Nao foi possivel realizar o parser do GravaCampo!")
            lRet := .F.
        EndIf

    EndIf

    If !Empty(Self:Empresa) .And. !Empty(cBkpEmp)
        RpcClearEnv()
        RpcSetType(3)
        RpcSetEnv(cBkpEmp)
    EndIf

Return lRet

//-------------------------------------------------------------------
/*/{Protheus.doc} LoadEmp
Recupera as empresas
@author jose.camilo
@since 29/07/2017
@wsmethod LoadEmp
@verbo GET
@receiver
@return logico + mensagem
/*/
WSRESTFUL LoadEmp DESCRIPTION "Recupera as empresas" FORMAT "application/json"

    WSMETHOD GET  DESCRIPTION "Recupera as empresas" 	PRODUCES APPLICATION_JSON
END WSRESTFUL

WSMETHOD GET WSSERVICE LoadEmp

    Local lRet      := .T.
    Local oJson     := JsonUtil():New()
    Local aEmpresas := {}
    Local aObj      := {}
    Local oItem     := Nil
    Local nI        := 0

    DbSelectArea("SM0")
    SM0->(DbGoTop())
    SM0->(DbEval({|| Iif(aScan(aEmpresas,{|x| x[1] ==  M0_CODIGO})==0,aAdd(aEmpresas,{M0_CODIGO,AllTrim(M0_NOME)}),Nil)  }))

    //Retorno dos itens
    for nI:= 1 to Len(aEmpresas)
        oItem := JsonUtil():New()
        oItem:PutVal("codigo", aEmpresas[nI][1])
        oItem:PutVal("nome", aEmpresas[nI][2])
        aadd(aObj,oItem)
    next nI

    oJson:PutVal("result", lRet)
    oJson:PutVal("msg","Empresas do ambiente")
    oJson:PutVal("obj", aObj)

    Self:SetResponse( oJson:ToJson() )  

Return lRet

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
