# AVMUD
Acelerador Verificação de Mudança

* [AVMUD-NodeJS](https://github.com/marllonfernandes/AVMUD-NodeJS) - Responsável pelo back-end do AVMUD. Sendo também mediador de requisições entre o AppMobile para o MongoDb e API do Protheus.
* [AVMUD-Mobile](https://github.com/JoseCamilo/AVMUD-Mobile) - Responsável pelo front-end do AVMUD em formato de Aplicação Mobile.
* [AVMUD-Protheus](https://github.com/JoseCamilo/AVMUD-Protheus) - Responsável por disponibilizar APIs de funcionalidades que serão executadas no Protheus.

## AVMUD-Protheus
### Validação de Dicionário
#### Parâmetro 
##### /VerificaSX6/ - GET
Verifica se o parâmetro está cadastrado com os valores desejados
* Parâmetros:
```
parametro - string: Nome do parâmetro à ser verificado
conteudo - string: Conteudo Protuguês que deveria estar gravado
contspa - string: Conteudo Espanhol que deveria estar gravado
conteng	- string: Conteudo Inglês que deveria estar gravado
```
* Retorno:
```
/VerificaSX6?parametro=MV_1DUP&conteudo=1&contspa=1&conteng=1
{
    "status": false,
    "sx6": [
        {
            "x6_conteudo": "001",
            "status": false
        },
        {
            "x6_contspa": "1",
            "status": true
        },
        {
            "x6_conteng": "1",
            "status": true
        }
    ]
}
```
#### Campo 
##### /VerificaCampo/ - GET
Verifica se um campo existe no ambiente. Do tipo físico, verifica também a existência fisicamente na tabela.
* Parâmetros:
```
campo - string: Campo que será verificado
```
* Retorno:
```
/VerificaCampo?campo=A1_COD
{
    "result": true,
    "msg": "Campo existe fisicamente"
}
```

### Validação de Arquivos 
#### /VerificaFile/ - GET
Verifica a existência de um arquivo no servidor do Protheus à partir do RootPath  
* Parâmetros:
```
caminho - string: Caminho completo, à partir do RootPath e com extensão, do arquivo à ser verificado
```
* Retorno:
```
/verificafile?caminho=/tecr120.prt
{
    "result": true
}
```

### Validação de Fontes 
#### /VerificaFonte/ - GET
Verifica se uma versão de fonte, comitada no TFS da TOTVS, está compilado no ambiente
* Parâmetros:
```
collection - string: Nome da Collection do TFS onde o fonte foi comitdado
arquivo - string: Caminho completo do arquivo fonte
changeset - string: Numero de registro do TFS changeSet
```
* Retorno:
```
/VerificaFonte?Collection=TI&ChangeSet=38195&Arquivo=$/SSIM/Fontes_Doc/Sustentacao/Fontes/Genericas/SIMXTMKB.PRW
{
    "result": true,
    "itens": [
        {
            "fonte": "SIMXTMKB.PRW",
            "status": "Compilado"
        }
    ]
}
```  
#### /VerificaChangeSet/ - GET
Verifica se um conjunto de fontes, comitado no TFS da TOTVS, está compilado no ambiente
* Parâmetros:
```
collection - string: Nome da Collection do TFS onde o fonte foi comitdado
changeset - string: Numero de registro do TFS changeSet
```
* Retorno:
```
/VerificaChangeSet?changeset=54425&collection=TDITOTVS12
{
    "result": true,
    "itens": [
        {
            "fonte": "TCRMA080.prw",
            "status": "Compilado"
        },
        {
            "fonte": "TCRMX102.prw",
            "status": "Compilado"
        },
        {
            "fonte": "TCRMR014.prw",
            "status": "Compilado"
        },
        {
            "fonte": "TCRMW001.prw",
            "status": "Compilado"
        },
        {
            "fonte": "FT300SPR.prw",
            "status": "Compilado"
        },
        {
            "fonte": "FT600CL.prw",
            "status": "Compilado"
        },
        {
            "fonte": "FT600FGR.prw",
            "status": "Compilado"
        },
        {
            "fonte": "FT600IMP.PRW",
            "status": "Compilado"
        },
        {
            "fonte": "FT600TOK.PRW",
            "status": "Compilado"
        }
    ]
}
``` 

#### /VerificaAlias/ - GET
Verifica se uma tabela existe no ambiente forçando sua criação, retorna qualquer erro na operação
* Parâmetros:
```
alias - string: Nome da Tabela que será verificada
```
* Retorno:
```
/VerificaAlias?alias=SA1
{
    "result": true,
    "msg": "Tabela existe"
}
```

```
http://172.16.93.182:8089/rest/EstruturaSxs?tipo=consulta&valor=1AA
{
    "result": true,
    "msg": "Estrutura encontrada",
    "estrutura": [
        {
            "XB_ALIAS": "1AA",
            "XB_TIPO": "1",
            "XB_SEQ": "01",
            "XB_COLUNA": "DB",
            "XB_DESCRI": "VENDEDORES",
            "XB_DESCSPA": "VENDEDORES",
            "XB_DESCENG": "SALES REPRESENTATIVE",
            "XB_CONTEM": "VAI",
            "XB_WCONTEM": ""
        },
        {
            "XB_ALIAS": "1AA",
            "XB_TIPO": "2",
            "XB_SEQ": "01",
            "XB_COLUNA": "06",
            "XB_DESCRI": "Cod. Vendedor",
            "XB_DESCSPA": "Cod. Vendedor",
            "XB_DESCENG": "Sales Rep. Code",
            "XB_CONTEM": "",
            "XB_WCONTEM": ""
        },
        {
            "XB_ALIAS": "1AA",
            "XB_TIPO": "4",
            "XB_SEQ": "01",
            "XB_COLUNA": "01",
            "XB_DESCRI": "Vendedor",
            "XB_DESCSPA": "Vendedor",
            "XB_DESCENG": "Sales Represent.",
            "XB_CONTEM": "VAI_CODVEN",
            "XB_WCONTEM": ""
        },
        {
            "XB_ALIAS": "1AA",
            "XB_TIPO": "4",
            "XB_SEQ": "01",
            "XB_COLUNA": "02",
            "XB_DESCRI": "Nome",
            "XB_DESCSPA": "Nombre",
            "XB_DESCENG": "Name",
            "XB_CONTEM": "VAI_NOMUSU",
            "XB_WCONTEM": ""
        },
        {
            "XB_ALIAS": "1AA",
            "XB_TIPO": "5",
            "XB_SEQ": "01",
            "XB_COLUNA": "",
            "XB_DESCRI": "",
            "XB_DESCSPA": "",
            "XB_DESCENG": "",
            "XB_CONTEM": "VAI->VAI_CODVEN",
            "XB_WCONTEM": ""
        }
    ]
}



http://172.16.93.182:8089/rest/EstruturaSxs?tipo=parametro&valor=PS_MSGEXP3&filial=01
{
    "result": true,
    "msg": "Estrutura encontrada",
    "estrutura": [
        {
            "X6_FIL": "01",
            "X6_VAR": "PS_MSGEXP3",
            "X6_TIPO": "C",
            "X6_DESCRIC": "Mensagem de Expedicao chamado Associado",
            "X6_DSCSPA": "",
            "X6_DSCENG": "",
            "X6_DESC1": "",
            "X6_DSCSPA1": "",
            "X6_DSCENG1": "",
            "X6_DESC2": "",
            "X6_DSCSPA2": "",
            "X6_DSCENG2": "",
            "X6_CONTEUD": "Chamado associado a um chamado planejado para liberacao do pacote no Portal de Clientes no dia #DTEXP",
            "X6_CONTSPA": "",
            "X6_CONTENG": "",
            "X6_PROPRI": "U",
            "X6_PYME": "",
            "X6_VALID": "",
            "X6_INIT": "",
            "X6_DEFPOR": "",
            "X6_DEFSPA": "",
            "X6_DEFENG": ""
        }
    ]
}


http://172.16.93.182:8089/rest/EstruturaSxs?tipo=campo&valor=A1_COD
{
    "result": true,
    "msg": "Estrutura encontrada",
    "estrutura": [
        {
            "X3_ARQUIVO": "SA1",
            "X3_ORDEM": "02",
            "X3_CAMPO": "A1_COD",
            "X3_TIPO": "C",
            "X3_TAMANHO": 6,
            "X3_DECIMAL": 0,
            "X3_TITULO": "Codigo",
            "X3_TITSPA": "Codigo",
            "X3_TITENG": "Code",
            "X3_DESCRIC": "Codigo do Cliente",
            "X3_DESCSPA": "Codigo del Cliente",
            "X3_DESCENG": "Customer\ufffds Code",
            "X3_PICTURE": "@!",
            "X3_VALID": "IIF(Empty(M->A1_LOJA),.T.,ExistChav('SA1',M->A1_COD+M->A1_LOJA,,'EXISTCLI'))",
            "X3_USADO": "gICAgICAgICAgICAgICw",
            "X3_RELACAO": "",
            "X3_F3": "",
            "X3_NIVEL": 1,
            "X3_RESERV": "g4A=",
            "X3_CHECK": "",
            "X3_TRIGGER": "",
            "X3_PROPRI": "",
            "X3_BROWSE": "S",
            "X3_VISUAL": "",
            "X3_CONTEXT": "",
            "X3_OBRIGAT": "",
            "X3_VLDUSER": "",
            "X3_CBOX": "",
            "X3_CBOXSPA": "",
            "X3_CBOXENG": "",
            "X3_PICTVAR": "",
            "X3_WHEN": "",
            "X3_INIBRW": "",
            "X3_GRPSXG": "001",
            "X3_FOLDER": "1",
            "X3_PYME": "S",
            "X3_CONDSQL": "",
            "X3_CHKSQL": "",
            "X3_IDXSRV": "S",
            "X3_ORTOGRA": "N",
            "X3_IDXFLD": "N",
            "X3_TELA": "",
            "X3_AGRUP": "",
            "X3_POSLGT": "",
            "X3_MODAL": ""
        },
        {
            "X3_ARQUIVO": "SA1",
            "X3_ORDEM": "02",
            "X3_CAMPO": "A1_COD",
            "X3_TIPO": "C",
            "X3_TAMANHO": 6,
            "X3_DECIMAL": 0,
            "X3_TITULO": "Codigo",
            "X3_TITSPA": "Codigo",
            "X3_TITENG": "Code",
            "X3_DESCRIC": "Codigo do Cliente",
            "X3_DESCSPA": "Codigo del Cliente",
            "X3_DESCENG": "Customer\ufffds Code",
            "X3_PICTURE": "@!",
            "X3_VALID": "IIF(Empty(M->A1_LOJA),.T.,ExistChav('SA1',M->A1_COD+M->A1_LOJA,,'EXISTCLI'))",
            "X3_USADO": "gICAgICAgICAgICAgICw",
            "X3_RELACAO": "A030INICPD()",
            "X3_F3": "",
            "X3_NIVEL": 1,
            "X3_RESERV": "g4A=",
            "X3_CHECK": "",
            "X3_TRIGGER": "",
            "X3_PROPRI": "",
            "X3_BROWSE": "S",
            "X3_VISUAL": "A",
            "X3_CONTEXT": "R",
            "X3_OBRIGAT": "",
            "X3_VLDUSER": "",
            "X3_CBOX": "",
            "X3_CBOXSPA": "",
            "X3_CBOXENG": "",
            "X3_PICTVAR": "",
            "X3_WHEN": "A030WHEN()",
            "X3_INIBRW": "",
            "X3_GRPSXG": "001",
            "X3_FOLDER": "1",
            "X3_PYME": "S",
            "X3_CONDSQL": "",
            "X3_CHKSQL": "",
            "X3_IDXSRV": "S",
            "X3_ORTOGRA": "N",
            "X3_IDXFLD": "N",
            "X3_TELA": "",
            "X3_AGRUP": "",
            "X3_POSLGT": "1",
            "X3_MODAL": "1"
        }
    ]
}



http://172.16.93.182:8089/rest/EstruturaSxs?tipo=tabela&valor=SA1
{
    "result": true,
    "msg": "Estrutura encontrada",
    "estrutura": [
        {
            "X2_CHAVE": "SA1",
            "X2_PATH": "",
            "X2_ARQUIVO": "SA1AA0",
            "X2_NOME": "Clientes",
            "X2_NOMESPA": "Clientes",
            "X2_NOMEENG": "Customers",
            "X2_ROTINA": "",
            "X2_MODO": "C",
            "X2_MODOUN": "E",
            "X2_MODOEMP": "E",
            "X2_DELET": 0,
            "X2_TTS": "",
            "X2_UNICO": "A1_FILIAL+A1_COD+A1_LOJA",
            "X2_PYME": "S",
            "X2_MODULO": 5,
            "X2_DISPLAY": "A1_COD+A1_LOJA+A1_NOME",
            "X2_SYSOBJ": "MATA030",
            "X2_USROBJ": "",
            "X2_POSLGT": "",
            "X2_CLOB": "",
            "X2_AUTREC": "",
            "X2_TAMFIL": 0,
            "X2_TAMUN": 0,
            "X2_TAMEMP": 0
        }
    ]
}


http://172.16.93.182:8089/rest/EstruturaSxs?tipo=consulta&valor=1AA
{
    "result": true,
    "msg": "Estrutura encontrada",
    "estrutura": [
        {
            "XB_ALIAS": "1AA",
            "XB_TIPO": "1",
            "XB_SEQ": "01",
            "XB_COLUNA": "DB",
            "XB_DESCRI": "VENDEDORES",
            "XB_DESCSPA": "VENDEDORES",
            "XB_DESCENG": "SALES REPRESENTATIVE",
            "XB_CONTEM": "VAI",
            "XB_WCONTEM": ""
        },
        {
            "XB_ALIAS": "1AA",
            "XB_TIPO": "2",
            "XB_SEQ": "01",
            "XB_COLUNA": "06",
            "XB_DESCRI": "Cod. Vendedor",
            "XB_DESCSPA": "Cod. Vendedor",
            "XB_DESCENG": "Sales Rep. Code",
            "XB_CONTEM": "",
            "XB_WCONTEM": ""
        },
        {
            "XB_ALIAS": "1AA",
            "XB_TIPO": "4",
            "XB_SEQ": "01",
            "XB_COLUNA": "01",
            "XB_DESCRI": "Vendedor",
            "XB_DESCSPA": "Vendedor",
            "XB_DESCENG": "Sales Represent.",
            "XB_CONTEM": "VAI_CODVEN",
            "XB_WCONTEM": ""
        },
        {
            "XB_ALIAS": "1AA",
            "XB_TIPO": "4",
            "XB_SEQ": "01",
            "XB_COLUNA": "02",
            "XB_DESCRI": "Nome",
            "XB_DESCSPA": "Nombre",
            "XB_DESCENG": "Name",
            "XB_CONTEM": "VAI_NOMUSU",
            "XB_WCONTEM": ""
        },
        {
            "XB_ALIAS": "1AA",
            "XB_TIPO": "5",
            "XB_SEQ": "01",
            "XB_COLUNA": "",
            "XB_DESCRI": "",
            "XB_DESCSPA": "",
            "XB_DESCENG": "",
            "XB_CONTEM": "VAI->VAI_CODVEN",
            "XB_WCONTEM": ""
        }
    ]
}





http://172.16.93.182:8089/rest/EstruturaSxs?tipo=indice&valor=SA1&ordem=1
{
    "result": true,
    "msg": "Estrutura encontrada",
    "estrutura": [
        {
            "INDICE": "SA1",
            "ORDEM": "1",
            "CHAVE": "A1_FILIAL+A1_COD+A1_LOJA",
            "DESCRICAO": "Codigo + Loja",
            "DESCSPA": "Codigo + Tienda",
            "DESCENG": "Code + Unit",
            "PROPRI": "S",
            "F3": "",
            "NICKNAME": "",
            "SHOWPESQ": "S"
        }
    ]
}




http://172.16.93.182:8089/rest/EstruturaSxs?tipo=gatilho&valor=AB3_LOJA&sequencia=002
{
    "result": true,
    "msg": "Estrutura encontrada",
    "estrutura": [
        {
            "X7_CAMPO": "AB3_LOJA",
            "X7_SEQUENC": "002",
            "X7_REGRA": "SA1->A1_DESC",
            "X7_CDOMIN": "AB3_DESC1",
            "X7_TIPO": "P",
            "X7_SEEK": "S",
            "X7_ALIAS": "SA1",
            "X7_ORDEM": 1,
            "X7_CHAVE": "xFilial('SA1')+M->AB3_CODCLI+M->AB3_LOJA",
            "X7_CONDIC": "",
            "X7_PROPRI": "S"
        }
    ]
}



http://172.16.93.182:8089/rest/NomeCampos?tabela=SX3
{
    "result": true,
    "msg": "Consulta realizada",
    "campos": [
        "X3_ARQUIVO",
        "X3_ORDEM",
        "X3_CAMPO",
        "X3_TIPO",
        "X3_TAMANHO",
        "X3_DECIMAL",
        "X3_TITULO",
        "X3_TITSPA",
        "X3_TITENG",
        "X3_DESCRIC",
        "X3_DESCSPA",
        "X3_DESCENG",
        "X3_PICTURE",
        "X3_VALID",
        "X3_USADO",
        "X3_RELACAO",
        "X3_F3",
        "X3_NIVEL",
        "X3_RESERV",
        "X3_CHECK",
        "X3_TRIGGER",
        "X3_PROPRI",
        "X3_BROWSE",
        "X3_VISUAL",
        "X3_CONTEXT",
        "X3_OBRIGAT",
        "X3_VLDUSER",
        "X3_CBOX",
        "X3_CBOXSPA",
        "X3_CBOXENG",
        "X3_PICTVAR",
        "X3_WHEN",
        "X3_INIBRW",
        "X3_GRPSXG",
        "X3_FOLDER",
        "X3_PYME",
        "X3_CONDSQL",
        "X3_CHKSQL",
        "X3_IDXSRV",
        "X3_ORTOGRA",
        "X3_IDXFLD",
        "X3_TELA",
        "X3_AGRUP",
        "X3_POSLGT",
        "X3_MODAL"
    ]
}

http://172.16.93.182:8089/rest/AtributoCampo?campo=PF9_MODULO&valor=VAZIO() .OR. EXISTCPO("PF7")&atributo=X3_VLDUSER
{
    "result": true,
    "msg": "Atributo de campo atualizado"
}

http://172.16.93.182:8089/rest/VerificaConsulta?consulta=AA1
{
    "result": true,
    "msg": "Consulta existe"
}

http://172.16.93.182:8089/rest/VerificaGatilho?gatilho=AA3_CBASE&&sequencia=003
{
    "result": true,
    "msg": "Gatilho existe"
}


http://172.16.93.182:8089/rest/GravaCampo
{
	"Origem" : "http://172.16.93.148:8083/rest",
	"Campo" : "A1_COD"
}
{
    "result": true,
    "msg": "Copia realizada"
}


http://172.16.93.182:8089/rest/GravaAtributoCampo
{
	"Origem" : "http://172.16.93.148:8083/rest",
	"Campo" : "PNS_CLIATV",
	"Atributo" : "X3_TAMANHO"
}
{
    "result": true,
    "msg": "Copia realizada"
}



http://172.16.93.182:8089/rest/GravaParametro
{
	"Origem" : "http://172.16.93.148:8083/rest",
	"Parametro" : "MV_CIDADE",
	"Filial" : "00302000500"
}
{
    "result": true,
    "msg": "Copia realizada"
}


http://172.16.93.182:8089/rest/GravaAtributoParametro
{
	"Origem" : "http://172.16.93.148:8083/rest",
	"Parametro" : "TI_FIL774",
	"Filial" : "",
	"Atributo" : "X6_DESC2"
}
{
    "result": true,
    "msg": "Copia realizada"
}


http://172.16.93.182:8089/rest/GravaGatilho
{
	"Origem" : "http://172.16.93.148:8083/rest",
	"Gatilho" : "TUU_TIPO",
	"Sequencia" : "013"
}
{
    "result": true,
    "msg": "Copia realizada"
}

http://172.16.93.182:8089/rest/GravaConsulta
{
	"Origem" : "http://172.16.93.148:8083/rest",
	"Consulta" : "CNBPRP"
}
{
    "result": true,
    "msg": "Copia realizada"
}


```
