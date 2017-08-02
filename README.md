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

# Contribua :D
Itens do escopo que ainda precisamos desenvolver:

##### Campo
- Um metodo Rest do verbo Get que, receba o endereço de 1 base Protheus(base onde o campo virtual esta criado) e o nome de um campo Virtual, busque o dicionario deste campo na base passada(usando o rest criado acima) e crie na base corrente. Retorno logico.


##### Atributo de Campo
- Um metodo Rest do verbo Get que receba um campo, um dado e um campo da tabela SX3, e confira no dicionário se o dado está gravado no dicionario no campo do X3. Retorno logico.
- Um metodo Rest do Verbo Get que, receba o endereço de 1 base Protheus(base onde o atributo esta correto), o nome do Campo(fisico ou virtual) e o nome de um campo do SX3, busque o conteudo deste campo na base passada(usando o rest acima) e grave na base corrente. Retorno logico.

##### Parametro
- Um metodo Rest do Verbo Get que, receba o endereço de 1 base Protheus(base que tem o parametro atualizado) e o nome de um parametro, busque o conteudo deste parametro na base passada(usando o rest acima) e grave na base corrente. Retorno logico.

##### Gatilho 
- Um metodo Rest do Verbo Get que, receba o nome de um campo que contenha Gatilho e o numero de Sequencia do gatilho, e verifica a existencia do gatilho. Retorno logico.
- Um metodo Rest do Verbo Get que, receba o endereço de 1 base Protheus(base que tem o gatilho atualizado) e o nome de um campo que contenha Gatilho e o numero de Sequencia do gatilho, busque o gatilho na base passada(usando o rest acima) e grave na base corrente. Retorno logico.

##### Consulta padrão
- Um metodo Rest do Verbo Get que, receba o nome de uma consulta padrão, e verifica sua existencia no ambiente. Retorno logico.
- Um metodo Rest do Verbo Get que, receba o endereço de 1 base Protheus(base que tem a consulta padrao atualizada) e o nome da consulta padrão, busque a consulta padrao na base passada(usando o rest acima) e grave na base corrente. Retorno logico.
