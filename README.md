# AVMUD
Acelerador Verificação de Mudança

* [AVMUD-NodeJS](https://github.com/marllonfernandes/AVMUD-NodeJS) - Responsável pelo back-end do AVMUD. Sendo também mediador de requisições entre o AppMobile para o MongoDb e API do Protheus.
* [AVMUD-Mobile](https://github.com/JoseCamilo/AVMUD-Mobile) - Responsável pelo front-end do AVMUD em formato de Aplicação Mobile.
* [AVMUD-Protheus](https://github.com/JoseCamilo/AVMUD-Protheus) - Responsável por disponibilizar APIs de funcionalidades que serão executadas no Protheus.

## AVMUD-Protheus
### Validação de Dicionário
#### SX6 
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
#### Campo Físico 
##### /VerificaCampo/ - GET
Verifica se um campo físico existe em uma determinada tabela
* Parâmetros:
```
alias - string: Tabela onde será verificado o campo físico 
campo - string: Campo que será verificado
```
* Retorno:
```
/VerificaCampo?alias=SA1&campo=A1_COD
{
    "result": true
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
