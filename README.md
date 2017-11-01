# AVMUD
Acelerador Verificação de Mudança

* [AVMUD-NodeJS](https://github.com/marllonfernandes/AVMUD-NodeJS) - Responsável pelo back-end do AVMUD. Sendo também mediador de requisições entre o AppMobile para o MongoDb e API do Protheus.
* [AVMUD-Mobile](https://github.com/JoseCamilo/AVMUD-Mobile) - Responsável pelo front-end do AVMUD em formato de Aplicação Mobile.
* [AVMUD-Protheus](https://github.com/JoseCamilo/AVMUD-Protheus) - Responsável por disponibilizar APIs de funcionalidades que serão executadas no Protheus.

## AVMUD-Protheus
### Validação de Dicionário

#### Campo
##### /NomeCampos/ - GET
Retorna o nome de todos os campos de uma tabela
* Parâmetros:
```
tabela - string: Tabela onde os campos serão buscados
empresa - string: Codigo da empresa onde será executado
```
* Requisição:
```
/NomeCampos?tabela=ADF&empresa=00

{
    "result": true,
    "msg": "Consulta realizada",
    "obj": [
        "ADF_FILIAL",
        "ADF_CODIGO",
        "ADF_ITEM",
        "ADF_CODSU9",
		...
        "ADF__GDEST"
    ]
}
```
 
##### /VerificaCampo/ - GET
Verifica se um campo existe no ambiente. Do tipo físico, verifica também a existência fisicamente na tabela.
* Parâmetros:
```
campo - string: Campo que será verificado
empresa - string: Codigo da empresa onde será executado
```
* Requisição:
```
/VerificaCampo?campo=A1_COD&empresa=00

{
    "result": true,
    "msg": "Campo existe fisicamente",
    "obj": []
}
```

##### /GravaCampo/ - POST
Recupera um campo de uma base e grava no ambiente corrente
* Parâmetros:
```
origem - string: WebService Rest do ambiente de origem do campo
campo - string: Campo que será copiado
empresa - string: Codigo da empresa onde será executado
```
* Requisição:
```
/GravaCampo
{
	"Origem" : "http://localhost:8083/rest",
	"Campo" : "A1_COD",
    "empresa" : "00"
}

{
    "result": true,
    "msg": "Copia realizada",
    "obj": []
}
```

##### /VerificaAtributoCampo/ - GET
Verifica se o conteudo de um atributo de um campo, esta atualizado no ambiente
* Parâmetros:
```
campo - string: Campo que será verificado
atributo - string: Atributo (Campo do SX3), que será verificado no Campo passado
valor - string: valor que deveriar estar gravado no atributo do Campo passado
empresa - string: Codigo da empresa onde será executado
```
* Requisição:
```
/VerificaAtributoCampo?campo=PF9_MODULO&valor=VAZIO() .OR. EXISTCPO("PF7")&atributo=X3_VLDUSER&empresa=00

{
    "result": true,
    "msg": "Atributo de campo atualizado",
    "obj": []
}
```

##### /GravaAtributoCampo/ - PUT
Recupera o atributo de um campo em uma base e grava na corrente
* Parâmetros:
```
campo - string: Campo que será verificado
atributo - string: Atributo (Campo do SX3), que será verificado no Campo passado
origem - string: WebService Rest do ambiente de origem do campo
empresa - string: Codigo da empresa onde será executado
```
* Requisição:
```
/GravaAtributoCampo
{
	"Origem" : "http://localhost:8083/rest",
	"Campo" : "A1_COD",
	"Atributo" : "X3_TAMANHO",
    "empresa" : "00"
}

{
    "result": true,
    "msg": "Copia realizada",
    "obj": []
}
```


#### Parâmetro 
##### /VerificaParametro/ - GET
Verifica se o parâmetro está cadastrado com os valores desejados
* Parâmetros:
```
parametro - string: Nome do parâmetro à ser verificado
conteudo - string: Conteudo Português que deveria estar gravado
contspa - string: Conteudo Espanhol que deveria estar gravado
conteng	- string: Conteudo Inglês que deveria estar gravado
filial - string: Filial onde o parametro será pesquisado
empresa - string: Codigo da empresa onde será executado
```
* Requisição:
```
/VerificaParametro?parametro=MV_1DUP&conteud=A&contspa=A&conteng=A&filial=01&empresa=00

{
    "result": false,
    "msg": "Parâmetro com dados desatualizados",
    "obj": [
        {
            "x6_conteud": "A",
            "status": true
        },
        {
            "x6_contspa": "A",
            "status": true
        },
        {
            "x6_conteng": "",
            "status": false
        }
    ]
}
```

##### /GravaParametro/ - POST
Recupera um parametro em uma base de origem e grava no ambiente corrente
* Parâmetros:
```
parametro - string: Nome do parâmetro à ser copiado
filial - string: Filial onde o parametro será pesquisado
origem - string: WebService Rest do ambiente de origem do parametro
empresa - string: Codigo da empresa onde será executado
```
* Requisição:
```
/GravaParametro

{
	"Origem" : "http://localhost:8083/rest",
	"Parametro" : "MV_CIDADE",
	"Filial" : "00302000500",
    "empresa" : "00"
}

{
    "result": true,
    "msg": "Copia realizada",
    "obj": []
}
```

##### /GravaAtributoParametro/ - PUT
Recupera um atributo de um parametro em um ambiente de origem e grava no corrente
* Parâmetros:
```
parametro - string: Nome do parâmetro à ser copiado
filial - string: Filial onde o parametro será pesquisado
atributo - string: Nome do campo do dicionario SX6 que será copiado
origem - string: WebService Rest do ambiente de origem do parametro
empresa - string: Codigo da empresa onde será executado
```
* Requisição:
```
/GravaAtributoParametro

{
	"Origem" : "http://localhost:8083/rest",
	"Parametro" : "TI_FIL774",
	"Filial" : "",
	"Atributo" : "X6_DESC2",
    "empresa" : "00"
}

{
    "result": true,
    "msg": "Copia realizada",
    "obj": []
}
```
#### Consulta
##### /VerificaConsulta/ - GET
Verifica se uma consulta padrao sxb, existe no ambiente
* Parâmetros:
```
consulta - string: nome da consulta que será verificada
empresa - string: Codigo da empresa onde será executado
```
* Requisição:
```
/VerificaConsulta?consulta=AA1&empresa=00

{
    "result": true,
    "msg": "Consulta existe",
    "obj": []
}
```

##### /GravaConsulta/ - POST
Recupera uma consulta padrao sxb de um ambiente de origem e grava no corrente
* Parâmetros:
```
consulta - string: nome da consulta que será verificada
origem - string: WebService Rest do ambiente de origem do parametro
empresa - string: Codigo da empresa onde será executado
```
* Requisição:
```
/GravaConsulta

{
	"Origem" : "http://localhost:8083/rest",
	"Consulta" : "CNBPRP",
    "empresa" : "00"
}

{
    "result": true,
    "msg": "Copia realizada",
    "obj": []
}
```
#### Gatilho
##### /VerificaGatilho/ - GET
Verifica se um gatilho existe no ambiente
* Parâmetros:
```
gatilho - string: nome do campo que realiza o gatilho que será verificado
sequencia - string: numero da sequencia do gatilho
empresa - string: Codigo da empresa onde será executado
```
* Requisição:
```
/VerificaGatilho?gatilho=AA3_CBASE&&sequencia=003&empresa=00

{
    "result": true,
    "msg": "Gatilho existe",
    "obj": []
}
```

##### /GravaGatilho/ - POST
Recupera um gatilho de uma base de origem e grava na corrente
* Parâmetros:
```
gatilho - string: nome do campo que realiza o gatilho que será verificado
sequencia - string: numero da sequencia do gatilho
origem - string: WebService Rest do ambiente de origem do gatilho
empresa - string: Codigo da empresa onde será executado
```
* Requisição:
```
/GravaGatilho

{
	"Origem" : "http://localhost:8083/rest",
	"Gatilho" : "TUU_TIPO",
	"Sequencia" : "013",
    "empresa" : "00"
}

{
    "result": true,
    "msg": "Copia realizada",
    "obj": []
}
```

#### Tabela
##### /VerificaAlias/ - GET
Verifica se a tabela existe no ambiente e se possivel a cria
* Parâmetros:
```
alias - string: tabela que será verificada
empresa - string: Codigo da empresa onde será executado
```
* Requisição:
```
/VerificaAlias?alias=SA1&empresa=00

{
    "result": true,
    "msg": "Tabela existe no ambiente",
    "obj": []
}
```

#### Indice
##### /VerificaIndice/ - GET
Verifica a existencia do indice fisicamente na tabela
* Parâmetros:
```
alias - string: tabela que será verificada com o indice
order - string: numero da ordem do indice na tabela
ou
nickname - string: apelido do indice na tabela
empresa - string: Codigo da empresa onde será executado
```
* Requisição:
```
/VerificaIndice?alias=SA1&order=1&empresa=00
/VerificaIndice?alias=SA9&nickName=SA9CARGO&empresa=00
{
    "result": true,
    "msg": "Indice existe",
    "obj": []
}
```

#### Estrutura de dicinonário
##### /EstruturaSxs/ - GET
Retorna a estrutura de um dicionario
* Parâmetros:
```
Tipo - string: Tipo da estrutura solicitada: tabela,campo,parametro,gatilho,indice ou consulta
Valor - string: Dado principal da pesquisa : nome da tabela ou campo ou parametro
Filial - string: Filial usada para pesquisar quando Tipo for parametro
Ordem - string: Ordem usada para pesquisar quando o Tipo for indice
Sequencia - string: Sequencia usada para pesquisar quando o Tipo for gatilho
empresa - string: Codigo da empresa onde será executado
```
* Requisição:
```
/EstruturaSxs?tipo=consulta&valor=1AA&empresa=00
{
    "result": true,
    "msg": "Estrutura encontrada",
    "obj": [
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
		...
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
```

### Validação de Fontes 
#### /VerificaFonte/ - GET
Verifica se uma versão de fonte, comitada no TFS da TOTVS, está compilado no ambiente
* Parâmetros:
```
collection - string: Nome da Collection do TFS onde o fonte foi comitdado
arquivo - string: Caminho completo do arquivo fonte
changeset - string: Numero de registro do TFS changeSet
ambiente - string: Nome do Ambiente onde será executado
```
* Requisição:
```
/VerificaFonte?Collection=TI&ChangeSet=38195&Arquivo=$/SSIM/Fontes_Doc/Sustentacao/Fontes/Genericas/SIMXTMKB.PRW

{
    "result": true,
    "msg": "Fontes atualizados no ambiente",
    "obj": [
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
ambiente - string: Nome do Ambiente onde será executado
```
* Requisição:
```
/VerificaChangeSet?Collection=TI&ChangeSet=38195

{
    "result": false,
    "msg": "Pelo menos um dos fontes estão desatualizados no ambiente",
    "obj": [
        {
            "fonte": "SIMXTMKB.PRW",
            "status": "Compilado"
        },
        {
            "fonte": "PMSMON08.prw",
            "status": "Data Invalida no RPO"
        }
    ]
}
``` 

#### /ListaArqChangeSet/ - GET
Lista Fontes commitados de um ChangeSet
* Parâmetros:
```
collection - string: Nome da Collection do TFS onde o fonte foi comitdado
changeset - string: Numero de registro do TFS changeSet
```
* Requisição:
```
/ListaArqChangeSet?Collection=CD-SP&ChangeSet=381980

{
    "result": true,
    "msg": "Listagem de arquivos",
    "obj": [
        {
            "nome": "TMKA260.PRW",
            "path": "$/Protheus_Padrao/Fontes_Doc/Inovação/V12/V12.1.7-INOV/V12 na Totvs/CRM/TMKA260.PRW"
        },
        {
            "nome": "TMKA341.PRW",
            "path": "$/Protheus_Padrao/Fontes_Doc/Inovação/V12/V12.1.7-INOV/V12 na Totvs/CRM/TMKA341.PRW"
        },
        {
            "nome": "FATA530.PRW",
            "path": "$/Protheus_Padrao/Fontes_Doc/Inovação/V12/V12.1.7-INOV/V12 na Totvs/Materiais/FATA530.PRW"
        }
    ]
}
``` 

### Validação de Arquivos 
#### /VerificaFile/ - GET
Verifica a existência de um arquivo no servidor do Protheus à partir do RootPath  
* Parâmetros:
```
caminho - string: Caminho completo, à partir do RootPath e com extensão, do arquivo à ser verificado
```
* Requisição:
```
/verificafile?caminho=/tecr120.prt

{
    "result": true,
	"msg": "Arquivo existente",
    "obj": []
}
```

### Cópia de Dicionário
#### /dicFileCreate/ - PUT
Cria arquivo de dicionario com filtro e formato informado na requisição
* Parâmetros:
```
diretorio - string: caminho onde o arquivo será salvo
tipo - string: tipo do dicionario que será copiado: SX1, SX2, SX3, SX6, SX7, SXB ou SIX
estrutura - array: dados que serão usados para filtro com a condição 'ou' entre os elementos deste array
drive - string: driver (RDD) a ser usado para criar o arquivo: DBFCDX, DBFCDXADS, TOPCONN
empresa - string: Codigo da empresa onde será executado
```
* Requisição:
```
/dicFileCreate

{
	"diretorio" : "temp",
	"tipo" : "SX6",
	"estrutura" : ["MV_CIDADE"],
	"driver" : "DBFCDXADS",
    "empresa" : "00"
}

{
    "result": true,
    "msg": "arquivo copiado com sucesso para: temp/dicFileCreate_SX6_20170908.dbf",
    "obj": []
}
```

### Email
#### /EnviaEmail/ - GET
Envia um email pelas funções do Protheus (SSIM e TOTVS)
* Parâmetros:
```
destinatario - string: endereços de email que receberam o email
subject - string: assunto do email
body - string: corpo do email
```
* Requisição:
```
/EnviaEmail?Subject=assuntoABC&Destinatario=jose.camilo@totovs.com.br;marllon.fernandes@totvs.com.br&Body=email enviado pelo rest

{
    "result": true,
    "msg": "Email enviado",
    "obj": []
}
```

### Empresas
#### /LoadEmp/ - GET
Busca Empresas

* Requisição:
```
/LoadEmp

{
    "result": true,
    "msg": "Empresas do ambiente",
    "obj": [
        {
            "codigo": "00",
            "nome": "TOTVS BRASIL"
        },
		...
        {
            "codigo": "82",
            "nome": "TOTVS FRANQUICIAS MI ARGENTINA"
        }
    ]
}
```