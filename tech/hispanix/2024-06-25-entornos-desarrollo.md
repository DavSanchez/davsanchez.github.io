---
title: "Entornos de desarrollo reproducibles con Nix"
date: 2024-06-26
tags:
  - nix
  - tech
  - 🇪🇸
---
<!-- LTeX: language=es -->

![Imagen de cabecera del repositorio de Nix en GitHub](./nix.png)

Tras la introducción a Nix que vimos en el [artículo anterior](./2024-06-16-conociendo-nix.md), continuamos con uno de los casos de uso más potentes de esta herramienta.

> [!warning]
> De forma similar al [artículo anterior](./2024-06-16-conociendo-nix.md), a partir de aquí asumiremos que has instalado Nix y activado dos configuraciones opcionales que facilitan mucho la experiencia de usuario con Nix. Si usaste el instalador oficial, puedes activarlas individualmente para cada comando del terminal usando `nix --extra-experimental-features "nix-command flakes"`, o activarlas globalmente añadiendo lo siguiente a `~/.config/nix/nix.conf` o `/etc/nix/nix.conf`:
>
> > ```conf
> > experimental-features = nix-command flakes
> > ```
<!--  -->
> [!info]
> A no ser que se indique lo contrario, los ejemplos a continuación están hechos en un sistema **macOS** utilizando **zsh** como shell por defecto.

Imagina que tienes un proyecto de software en el que estás trabajando, quizá en las etapas iniciales. No tienes interés en empaquetar el producto en Nixpkgs ni nada parecido, porque quizá aún no tienes nada que lanzar aún, pero sí que te gustaría:

- Asegurarte de que tienes todas las herramientas que tienes para desarrollar este proyecto, como por ejemplo:
  - La versión *nightly* de Rust con varios objetivos de compilación diferentes al por defecto, como WASM u otros.
  - Una versión de Go específica, diferente a una que puedas tener instalada globalmente vía otras herramientas como Homebrew.
  - La CLI de AWS/Ansible/Terraform.
  - Alguna utilidad de Python que normalmente instalarías con `pip`, como `jmespath`.
  - Variables de entorno específicas fijadas a un valor particular (`AWS_PROFILE` o alguna configuración de git).
- Asegurarte de que tus *pipelines* de CI/CD tienen estas mismas herramientas, en las mismas versiones, para que tu equipo pueda ejecutar localmente las mismas acciones que la *pipeline* CI/CD y obtener el mismo resultado.
- Forzar un estilo particular de mensajes en los *commits*, o ejecutar las mismas herramientas (formateadores, *linters* o una herramienta propia) con la misma configuración cada vez que un miembro del equipo crea una revisión o *commit*.
- Prevenir que las herramientas que has instalado para tu proyecto entren en conflicto con otros proyectos de tu equipo, sin por ello tener que utilizar contenedores:
  - El Proyecto A usa algúna funcionalidad solo disponible en Go 1.20 y posterior.
  - El Proyecto B no está listo para actualizar más allá de Go 1.19 por ciertos problemas con CGO y `libresolv`.
- Hacer que cada nueva incorporación al equipo esté listo para usar todas estas herramientas y convenciones fácilmente.

¿Cómo podrías lograr todo esto sin recurrir a contenedores, Docker y demás?

Averigüémoslo.

Recuerda que una de las posibles [*salidas*](./2024-06-16-conociendo-nix.md) que pueden definirse en un *Nix flake* son **entornos de desarrollo**. Esto significa que, usando Nix, puedes ir al repositorio de tu proyecto, "convertirlo" en un *flake* (tan solo añadiendo un fichero `flake.nix` en su raíz), y con solo algunas instrucciones en el lenguaje Nix puedes configurar todo lo que mencionamos en la lista. Añade el fichero a tu control de versiones y todos los miembros de tu equipo, las *pipelines* CI/CD, todo el mundo podrá comenzar a usarlo.

No más "pues en mi máquina funciona" o instalar la misma herramienta con `apt-get` en Ubuntu, `Dockerfile` con `FROM ubuntu:latest` en macOS, una GitHub Action en tu CI/CD y golpearte la cabeza contra la pared cuando alguno de los tres entornos falla.

## Nuestro primer *flake*

> [!tip]
> Esta seguramente sea tu primera exposición al lenguaje de programación Nix. No entraremos en demasiado detalle por ahora, confiando en que no será difícil intuir lo que ocurre en los comienzos, pero si necesitas recursos para profundizar, echa un vistazo a [esta web interactiva](https://zaynetro.com/explainix) o al [tutorial oficial](https://nix.dev/tutorials/nix-language.html). ¡O no dudes en preguntarme!

### Estructura básica

Un fichero `flake.nix` contiene lo que en lenguaje Nix se denomina un **conjunto de atributos** (*attribute set* o *attrset* para abreviar), algo muy similar a un objeto JSON:

```nix
{
  description = "Nix flake para mi proyecto en Go";

  inputs = {}; # Omitimos los contenidos de momento

  outputs = {}; # Esto que lees es un comentario
}
```

Recordemos que Nix hace que nuestras *builds* sean declarativas y **reproducibles**. Cualquier salida que definamos vendrá determinada por las entradas que definamos, y por nada más. Veamos, entonces, cómo se definen estos *inputs* (entradas) y *outputs* (salidas).

#### Entradas

Para declarar paquetes como dependencias, estos paquetes tienen que venir de alguna parte. ¿De dónde vienen, pues? ¿Quién los define?

La respuesta la tenemos en el artículo anterior, en el que mencioné que Nixpkgs es un repositorio de más de 100000 paquetes instalables. Declaremos entonces Nixpkgs, que también es un *flake*, como entrada:

```nix
{
  description = "Nix flake para mi proyecto en Go";

  inputs = {
    # Puedes darle un nombre cualquiera a tu input, lo que determina qué es es la URL.
    nixpkgs = {
      # Consulta el artículo anterior para recordar qué significaba esta referencia.
      url = "github:NixOS/nixpkgs/nixos-23.11";
    };
  };

  outputs = {};
}
```

Esta sintaxis es útil si queremos personalizar cada entrada, pero no cubriremos esto en este artículo. Si solo queremos añadir entradas de una forma básica, solo necesitando su URL, podemos hacer esto en su lugar:

```nix
{
  description = "Nix flake para mi proyecto en Go";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
  inputs.git-hooks.url = "github:cachix/git-hooks.nix";

  outputs = {};
}
```

O agrupar las entradas de esta otra forma:

```nix
{
  description = "Nix flake para mi proyecto en Go";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    git-hooks.url = "github:cachix/git-hooks.nix";
  };

  outputs = {};
}
```

#### Salidas

Aquí es donde se pone interesante. Las salidas definen lo que ofrece nuestro *flake*, y deberían utilizar de alguna forma las entradas que hemos definido anteriormente. Como ya hemos comentado, Nix recibe mucha influencia de la programación funcional, por tanto, **la salida está definida como una función que recibe las entradas como parámetro, y devuelve un nuevo conjunto de atributos**. Las funciones en Nix se definen con la forma `myFunction = arg1: arg2: ... argN: returned_value` (sí, esto es [currying](https://en.wikipedia.org/wiki/Currying)). Por tanto, para nuestra salida:

```nix
{
  description = "Nix flake para mi proyecto en Go";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    git-hooks.url = "github:cachix/git-hooks.nix";
  };

  outputs = {
    nixpkgs,
    git-hooks
  }: {}; # Esto último es el conjunto de atributos que devuelve la función. Vacío de momento.
}
```

Supón que hemos definido muchas entradas, pero solo un pequeño subconjunto de ellas es importante y queremos que exista una referencia fácil a ellas en todo momento, mientras que las demás solo se referenciarán ocasionalmente. Podemos des-estructurar el conjunto de atributos de la siguiente forma (que reconocerán los programadores de Haskell o Rust):

```nix
{
  description = "Nix flake para mi proyecto en Go";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    git-hooks.url = "github:cachix/git-hooks.nix";
  };

  outputs = inputs @ { # Vinculamos todas las entradas a "inputs".
    nixpkgs,           # Queremos referencia fácil a nixpkgs
    ...                # Omitimos lo demás
  }: {
                       # Ahora podemos usar las referencias aquí.
                       # Ya sea refiriéndose a "nixpkgs" directamente o,
                       # si queremos acceder a git-hooks, usando
                       # "inputs.git-hooks".
  };
}
```

##### Añadiendo dependencias

Ya tenemos la forma básica y queremos tener algo disponible rápidamente. Definamos nuestro entorno de desarrollo, una *shell* personalizada con el atributo [**devShell**](https://nixos.org/manual/nixpkgs/stable/#sec-pkgs-mkShell):

```nix
{
  description = "Nix flake para mi proyecto en Go";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    git-hooks.url = "github:cachix/git-hooks.nix";
  };

  outputs = {
    nixpkgs,
    ...
  }: let
    macM1Packages = nixpkgs.legacyPackages.aarch64-darwin;
    linuxAMD64Packages = nixpkgs.legacyPackages.x86_64-linux;
  in {
    devShells.aarch64-darwin.default = macM1Packages.mkShell {
      packages = [
        # Al momento de escribir esto, la versión de
        # Go_1_19 en nixpkgs/nixos-23.11 es 1.19.13
        macM1Packages.go_1_19
      ];
    };

    devShells.x86_64-linux.default = linuxAMD64Packages.mkShell {
      # Nada por ahora
    };
  };
}
```

###### ¿Es obligatorio definir *devShells* y usar referencias a los conjuntos de paquetes para cada sistema por separado?

**La respuesta rápida es *¡No!***.

La respuesta larga es que Nix sirve para construir paquetes, una herramienta o sistema de *builds*, y que por tanto es necesario poder ser consciente del sistema o *target* para el que estamos construyendo un paquete, *shell* o salida en general, pues en general un software no puede ser compilado sin más para todos los posibles sistemas que existen.

Dicho esto, para el caso que nos ocupa en este tutorial, combinaciones de sistema operativo y arquitectura bastante extendidas y de propósito general, sí que podemos asumir que cierto software pueda compilar para ellas sin demasiado problema. Otros desarrolladores han hecho el trabajo duro por nosotros y ofrecen algunas funciones y sistemas para minimizar la repetición, a cambio de un poquito más de código Nix. Una de estas utilidades es [`flake-utils`](https://github.com/numtide/flake-utils). Para utilizarla imagino que ya intuirás lo que necesitamos: Definirla como entrada.

```nix
{
  description = "Nix flake para mi proyecto en Go";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    git-hooks.url = "github:cachix/git-hooks.nix";
    flake-utils.url = "github:numtide/flake-utils"; # Nuevo input!
  };

  outputs = {
    nixpkgs,
    flake-utils,
    ...
  }: flake-utils.lib.eachDefaultSystem (system: let
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    devShells.default = pkgs.mkShell {
      packages = with pkgs; [
        go_1_19
        terraform
      ];
    };
  });
}
```

Este `eachDefaultSystem` que hemos utilizado es una función que recibe otra función como parámetro (ya hablé de la programación funcional), y esta función a su vez recibiría un parámetro que llamamos `system`, que utilizamos en el cuerpo de la función para definir el conjunto de paquetes que definirá nuestro *devShell*. ¿Qué argumento de `system` usa esa función? De eso se encarga `eachDefaultSystem`, que pasa una lista con los siguientes valores:

- `x86_64-linux`
- `aarch64-linux`
- `x86_64-darwin`
- `aarch64-darwin`

> [!tip]
> Para comenzar de forma simple he sugerido utilizar `flake-utils`, pero para algo más avanzado y modular, que uso actualmente en mis *flakes*, recomiendo [`flake-parts`](https://flake.parts).

Vamos a ver cuáles son las salidas de nuestro *flake* ejecutando `nix flake show` en el directorio raíz de tu *flake* (o, ya sabes, `nix flake show "path:/ruta/a/tu/flake"`). Nix tardará un tiempo en evaluar el código Nix de tu *flake*, descargar las entradas y evaluar las salidas, pero finalmente se mostrará algo parecido a esto:

```console
$ nix flake show
warning: creating lock file '/Users/david/random/flake.lock'
path:/Users/david/random?lastModified=1695943648&narHash=sha256-CbZ16gnffi0%2B9ig270ifpbHYOKS4CreKnH7GlPi
└───devShells
    ├───aarch64-darwin
    │   └───default: development environment 'nix-shell'
    ├───aarch64-linux
    │   └───default omitted (use '--all-systems' to show)
    ├───x86_64-darwin
    │   └───default omitted (use '--all-systems' to show)
    └───x86_64-linux
        └───default omitted (use '--all-systems' to show)
```

El fichero `flake.lock` es un *lockfile*, que tiene un propósito similar a los probablemente más conocidos `package-lock.json` para proyectos del ecosistema JavaScript, `Gemfile.lock` para Ruby, `Cargo.lock` para Rust, etc. Contendrá los *hashes* de todas las entradas de nuestro *flake*, asegurando la reproducibilidad e integridad de nuestras entradas.

#### Entrando en la *shell*

Entonces, ¿qué falta? ¿Cómo accedemos a la versión de Go que hemos declarado en este entorno de desarrollo? Podemos hacerlo con `nix develop`:

```console
$ which go
/opt/homebrew/opt/go # Ejemplo de una instalación de Go existente

$ go version
go version go1.20.8 darwin/arm64

$ nix develop
# Se tomará su tiempo, pero después...

$ which go
/nix/store/3yndvq32rxh6h9bqjd6n20npk2ix0ah2-go-1.19.13/bin/go

$ go version
go version go1.19.13 darwin/arm64
```

¡Listo! **Como si fuera un `virtualenv` de Python, pero para cualquier lenguaje y más**, usar `nix develop` nos introduce en una nueva *shell* con los paquetes que definimos disponibles en el `$PATH`. ¡De forma nativa! ¡Sin contenedores, volúmenes, etc!

Ahora escribe algo de código, compílalo[^go-compile], y ya hablaremos de cómo empaquetar tu programa de Go con Nix en un futuro artículo.

Para salir de esta *shell* y volver al estado anterior a utilizar `nix develop`, simplemente ejecuta `exit` o usa `Ctrl + D`.

Por supuesto, podrás intuir que esto no es lo único que se puede hacer en cuanto a definir *shells* o empaquetar software. Puedes hacer chequeos automatizados, usar *shell hooks* para ejecutar acciones al entrar en la *shell*, configurar *pre-commit hooks* para personalizar tu trabajo con `git`, referenciar otros ficheros dentro del *flake* (como una configuración en formato YAML) con `import`, definir *bundlers* y empaquetar el software para sistemas que no usen Nix... ¡Y mucho más!

> [!tip]
> Tampoco es completamente necesario entrar a picar código Nix y definir todo esto a mano. Aunque lo recomiendo para que saber más o menos lo que ocurre, hay muchas iniciativas para abstraer muchos de los mecanismos internos y comenzar a trabajar más rápidamente.
>
> El que puedo recomendar es [`devenv.sh`](https://devenv.sh), cuyo autor es un contribuidor principal del ecosistema (también autor de [`cachix`](https://www.cachix.org), `git-hooks.nix` y algunas GitHub Actions para instalar Nix en los *runners*). `devenv.sh` también se configura con el lenguaje Nix, pero de forma bastante sencilla.
>
> También hay alternativas que utilizan YAML o JSON, como [Flox](https://flox.dev) o [DevBox](https://www.jetify.com/devbox), respectivamente. Estos últimos apenas los conozco, y aunque pueden ofrecer algunas comodidades, en mi opinión parecen abstraer demasiado lo que ocurre y podrían no cubrir todos los casos de uso particulares.
>
> Como ves, el ecosistema es [muy amplio](https://nix-community.github.io/awesome-nix/).

Probablemente también tengas muchas preguntas. Trato de responder a algunas de ellas en el apartado siguiente, enlazando a recursos relevantes, aunque puedo desarrollar estos temas en futuros artículos o si me preguntas directamente.

## Algunas preguntas y respuestas rápidas

### ¿Cómo sé si un paquete que necesito está disponible en Nixpkgs?

Puedes buscar [aquí](https://search.nixos.org/packages). Si utilizas DuckDuckGo, puedes acceder a esta búsqueda rápidamente con `!nixpkgs`. Por ejemplo, usa `!nixpkgs python` para ver qué paquetes relacionados con Python hay en Nixpkgs.

### Trabajar con *devShells* descarga cosas en mi sistema. ¿Cómo limpio lo que ya no necesito?

Aunque puede dar la impresión de que no *instalas* nada de forma permanente en tu máquina cuando configuras todos estos entornos de desarrollo y descargas paquetes con Nix, obviamente todos estos recursos están en tu sistema y ocupan espacio.

Nix usa un recolector de basura para eliminar contenido de la Nix Store que no utilizas. Este recolector puede configurarse para ejecutar regularmente, o puedes llamarlo directamente con `nix store gc` (no necesitas estar dentro de un *flake* o un *devShell* para ello).

Como intuirás, hay mucho más detrás de esta operación. ¿Cómo puede Nix identificar lo que está en uso y lo que no? ¿Está esto relacionado con los enlaces simbólicos que se crearon cuando ejecutamos `nix build` en [el tutorial anterior](./2024-06-16-conociendo-nix.md)?

Puedes leer más [aquí](https://nixos.org/guides/nix-pills/11-garbage-collector.html) y [aquí](https://nixos.wiki/wiki/Storage_optimization).

### ¿Tengo que escribir siempre los *flakes* desde cero?

Existe un mecanismo para descargar plantillas, que también pueden exponerse como salidas de un *flake*. Por ejemplo, yo mantengo un *flake* con [algunas de ellas](https://github.com/DavSanchez/nix-dotfiles/tree/master/templates) (aunque no las he actualizado para que usen `flake-parts` aún).

- Para proyectos de Rust, con algunas utilidades para configurar las *toolchains* (utilizando [fenix]) y *git hooks*. Puedes descargarla con `nix flake init -t "github:DavSanchez/nix-dotfiles#rust_fenix_precommit"`.
- Para proyectos de Go, con *git hooks*, usa `nix flake init -t "github:DavSanchez/nix-dotfiles#go_precommit"`
- Para proyectos con Haskell y *git hooks*, usa `nix flake init -t "github:DavSanchez/nix-dotfiles#haskell_precommit"`.

### ¿Cómo configuro los *git hooks*?

Si leíste el apartado anterior, verás que menciono repetidamente los *git hooks*. En mi opinión son una herramienta muy útil para asegurarte de que tu código cumple ciertos estándares antes de realizar un *commit* (¿pasa los tests unitarios? ¿Está formateado?) o para usar algún formato en tus mensajes de *commit*, como [*conventional commits*](https://www.conventionalcommits.org/en/v1.0.0/).

Mis plantillas definen como entrada [`cachix/git-hooks.nix`](https://github.com/cachix/git-hooks.nix) para definir estos *hooks* con Nix y cargarlos directamente en tu *devShell* con *shellHook*. Echa un vistazo al repositorio de `git-hooks.nix` para ver qué *hooks* están disponibles y cómo crear los tuyos propios.

### ¿Cómo personalizo mis *devShells*?

Esto depende de tus preferencias personales. `nix develop` utiliza Bash por defecto. Yo utilizo [`zsh-nix-shell`](https://github.com/chisui/zsh-nix-shell), un plugin que permite usar ZSH como *devShell*, y también [`starship`](https://starship.rs/) para personalizar el *prompt*.

### ¿Tengo que ejecutar `nix develop` cada vez que quiero entrar en la *devShell*?

Puedes automatizar esto gracias a [`direnv`](https://direnv.net). Si echaste un vistazo a mis plantillas verás que incluyen un fichero `.envrc`. Este fichero ayuda a conseguir este comportamiento. Si me muevo dentro de un directorio que es un *flake* con `cd`, la *devShell* se activa automáticamente. Si hago `cd` y salgo de la *shell*, se desactiva.

Hay un artículo en el [blog de Determinate Systems](https://determinate.systems/posts/nix-direnv) con más detalles.

### ¿Qué más se puede hacer?

¿Podría usar Nix en mis líneas de CI/CD con [GitHub Actions](https://nix.dev/tutorials/nixos/continuous-integration-github-actions)? ¿Puedo configurar todo mi [sistema](https://github.com/DavSanchez/nix-dotfiles/) o mi [usuario](https://github.com/nix-community/home-manager) de forma declarativa con Nix? ¿Aunque esté en [macOS](https://github.com/LnL7/nix-darwin)? ¿Qué hay de [NixOS](https://search.nixos.org/options?query=options)?

Probablemente exploremos todas estas cosas en futuros artículos. De momento ahí tienes algunos enlaces a documentación y plantillas para ir abriendo boca.

¡Nos vemos en el siguiente!

[^go-compile]: Para los lenguajes compilados para Linux y que usen enlazado dinámico (como Go al habilitar CGO) hay ciertos detalles de distribución que no hemos cubierto en este artículo y que te puedes encontrar si avanzas más por tu cuenta. Si las rutas de todas las dependencias están en la Nix Store, dónde espera un binario con *dynamic linking*, generado con Nix, encontrar al *dynamic loader*? ¿Sigue siendo en rutas como `/lib64/ld-linux-x86-64.so.2`? 🙃
