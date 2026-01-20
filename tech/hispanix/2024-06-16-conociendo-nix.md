---
title: "Conociendo Nix"
date: 2024-06-16
tags:
  - nix
  - tech
  - 🇪🇸
---
<!-- LTeX: language=es -->

Como ya comentaba en [mi artículo anterior en español](./index.md), tengo algunos recursos sobre [Nix](https://nixos.org) que he ido compilando en mi trabajo, construyendo sobre la documentación oficial y otras fuentes de documentación. La idea es llevar este contenido a nuestro idioma para dar a conocer este proyecto, así que sin más, ahí va.

## ¿Qué es Nix?

![Imagen de cabecera del repositorio de Nix en GitHub](./nix.png)

Esencialmente, Nix es un sistema de *builds* y un gestor de paquetes, tal y como lo puede ser el gestor de paquetes de Ubuntu con el que interactuamos via `apt`, Homebrew para macOS o scoop para Windows. También se denomina Nix al lenguaje de programación que se usa para describir cómo este gestor de paquetes realiza su trabajo. Nix puede usarse tanto en Linux como en macOS (y en cierta medida en Windows, a través de WSL).

Además, está relacionado con otros dos proyectos:

- **Nixpkgs**: el principal repositorio de paquetes disponibles para su ser instalados con Nix, donde cada paquete y función está descrita con el lenguaje de programación Nix. El equivalente en Ubuntu sería el lugar donde se definen qué paquetes pueden instalarse con `apt`. Nixpkgs cuenta con más de 100000 paquetes que se actualizan regularmente.
- **NixOS**: un sistema operativo gestionado de forma declarativa, cuya configuración también se realiza con el lenguaje de programación Nix.

Nix está fuertemente inspirado en la programación funcional y anima a adoptar una forma de trabajar con *software* declarativa y reproducible, de la que emerge lo que podríamos denominar "DevOps funcional". La reproducibilidad se consigue en gran medida porque las *builds* de Nix se realizan de forma aislada, con sus dependencias explícitamente definidas, de forma que no pueda haber alguna dependencia oculta que influencie nuestra *build* (por ejemplo, una variable de entorno de nuestra *shell* fijada a un valor concreto).

En esta serie de artículos pretendo mostrar las ventajas que esto tiene y cómo puedes usarlo en tus proyectos.

> [!info]
> Puedes adoptar el uso de Nix de forma incremental, introduciéndolo en tu forma de trabajar gradualmente sin por ello sustituir tu gestor de paquetes predilecto.

## Instalando Nix

Ahora mismo hay dos formas de instalar Nix en tu sistema. El primero es [el instalador oficial](https://nixos.org/download), que ejecuta un script de Bash, y el segundo es [el instalador desarrollado por Determinate Systems (DetSys)](https://github.com/DeterminateSystems/nix-installer). DetSys es un contribuidor principal del ecosistema (el creador de Nix, Eelco Dolstra, trabaja para esta compañía), y su instalador consiste en un programa escrito en Rust con algunas funcionalidades adicionales:

- Activa dos configuraciones experimentales que tienen ya bastante madurez, mejoran la experiencia de trabajar con Nix y en general están bastante recomendadas: la [CLI unificada](https://zero-to-nix.com/concepts/nix#unified-cli) y [Nix Flakes](https://zero-to-nix.com/concepts/flakes) (la palabra *nix* significa nieve en latín, y la palabra *flake* significa copo en inglés).
- Puede deshacer los cambios realizados en tu sistema cuando instalas Nix, que dependen del sistema en cuestión y pueden [no ser triviales](https://nixos.org/manual/nix/unstable/installation/uninstall.html), facilitando el proceso de desinstalación.
- Es más portable (¡puedes instalar Nix en tu Steam Deck!)

Llevo utilizando Nix desde antes de que el instalador de DetSys estuviera disponible, por lo que no lo he probado a conciencia. El instalador oficial funciona bien igualmente, aunque parece que el objetivo a largo plazo es que la versión de DetSys se convierta en el instalador oficial de alguna forma, por lo que podría merecer la pena empezar a usarlo.

> [!info]
> Si no estás listo para hacer estos cambios en tu sistema, puedes probar Nix con Docker:
>
> - Ejecuta un contenedor de NixOS con `docker run -it nixos/nix`.
> - Ejecuta cualquier otro contenedor de Linux e instala Nix en él (el instalador de DetSys también está preparado para instalar sobre contenedores).
>

### ¿Qué se puede hacer con Nix?

> [!warning]
> A partir de este punto, asumiremos que has instalado Nix y activado las dos configuraciones experimentales mencionadas anteriormente. Si usaste el instalador oficial, puedes activarlas individualmente para cada comando del terminal usando `nix --extra-experimental-features "nix-command flakes"`, o activarlas globalmente añadiendo lo siguiente a `~/.config/nix/nix.conf` o `/etc/nix/nix.conf`:
>
> > ```conf
> > experimental-features = nix-command flakes
> > ```
>
> (Por supuesto, estas configuraciones pueden ser descritas declarativamente con Nix, pero aún no hemos llegado a eso).
<!--  -->
> [!info]
> A no ser que se indique lo contrario, los ejemplos a continuación están hechos en un sistema **macOS** utilizando **zsh** como *shell* por defecto.

Tras haber instalado Nix, prueba a ejecutar un programa[^cowsay-example] directamente, **sin instalarlo**:

```console
$ echo "Hello, Nix\!" | nix run "nixpkgs#cowsay"
 _____________
< Hello, Nix! >
 -------------
        \   ^__^
         \  (oo)\_______
            (__)\       )\/\
                ||----w |
                ||     ||
```

#### ¿Qué ha pasado?

Cuando ejecutas `nix run "nixpkgs#cowsay"` ocurre lo siguiente:

1. La CLI utilizará la **referencia** `nixpkgs`, trayendo su código escrito en Nix, y ejecuta su **salida** `cowsay`. Más adelante veremos lo que significa esto.
2. La salida `cowsay` representa un **paquete**, así que este paquete es construido o compilado, y el resultado de este proceso (el binario `cowsay` y sus dependencias, si las hay), se almacenan en una ubicación creada por Nix en el sistema: la **Nix Store**. La ubicación de la *store* es habitualmente `/nix/store`.
3. El binario ejecutable para `cowsay`, que es el resultado de construir el paquete `cowsay`, es ejecutado.

Veamos en qué ubicación exacta de la Nix Store está el resultado de la compilación de este paquete:

```console
$ nix build "nixpkgs#cowsay" --print-out-paths
/nix/store/yspq7q2as6pdg7jjaq1pphf81ym8ayy5-cowsay-3.7.0-man
/nix/store/qj79hvidddnc6z5nlpdw9bmybdmsjwpi-cowsay-3.7.0
```

Y parece que el resultado no es solo un binario, sino las *man pages* también. Veámoslo todo, también usando Nix para obtener un comando alternativo a `tree`:

```console
$ nix run "nixpkgs#tre-command" -- /nix/store/qj79hvidddnc6z5nlpdw9bmybdmsjwpi-cowsay-3.7.0/
/nix/store/qj79hvidddnc6z5nlpdw9bmybdmsjwpi-cowsay-3.7.0/
├── bin
│   ├── cowsay
│   └── cowthink
└── share
    └── cowsay
        ├── cows
        │   ├── satanic.cow
        │   ├── telebears.cow
       ... ...
        │   ├── luke-koala.cow
        │   └── moofasa.cow
        └── site-cows
```

```console
nix run "nixpkgs#tre-command" -- /nix/store/yspq7q2as6pdg7jjaq1pphf81ym8ayy5-cowsay-3.7.0-man
/nix/store/yspq7q2as6pdg7jjaq1pphf81ym8ayy5-cowsay-3.7.0-man
└── share
    └── man
        └── man1
            ├── cowthink.1.gz
            └── cowsay.1.gz
```

> [!info] ¿Por qué los *hashes* en la ruta?
> Las rutas de la Nix Store empiezan con un *hash* seguido del nombre y la versión de lo que contienen. Aunque no ahondaremos en esto en este artículo, este *hash* se calcula a partir de los parámetros de entrada del paquete (parámetros de la *build*, dependencias, versiones de estas dependencias, etc).
>
> Cualquier cambio en estos parámetros cambiará el valor del *hash*, por lo que cada ruta será **única**. Además, la Nix Store es un sistema de ficheros de sólo lectura, por lo que una vez se construye un paquete y se introduce en la Store, **no puede ser modificado**.
>
> Dado que Nix construye los paquetes de forma aislada, y el resultado de esta construcción solo depende de los parámetros de entrada, **usar los mismos parámetros de entrada siempre producirá la misma salida**. Esto tiene poderosas implicaciones en la distribución de software y el uso de cachés para acelerar los tiempos de compilación, que probablemente exploremos en futuros artículos.

También fíjate en lo siguiente. Cuando ejecutaste `nix build "nixpkgs#cowsay`, los resultados que se almacenaron en la Nix Store quedan también enlazados a una ubicación en el mismo directorio donde ejecutaste el comando, dentro de unos directorios llamados `result`:

```console
$ ls -lah result*
# Información irrelevante omitida
lrwxr-xr-x ... result ⇒ /nix/store/qj79hvidddnc6z5nlpdw9bmybdmsjwpi-cowsay-3.7.0
lrwxr-xr-x ... result-man ⇒ /nix/store/yspq7q2as6pdg7jjaq1pphf81ym8ayy5-cowsay-3.7.0-man
```

> [!info]
> Para evitar que esto pase, puedes usar `--no-link`.
<!--  -->
> [!tip]
> La información con la que me gustaría que te quedaras es que un binario que ejecutas via Nix será parte de un paquete, pero estos paquetes también pueden contener *man pages* u otros ficheros. Podría contener una configuración de `git` o de `vim`, que podríamos luego enlazar a `~/.gitconfig` o `~/.config/vim/`... pero esto lo exploraremos en otro artículo 😉.

##### Sobre las *referencias* y *salidas* mencionadas anteriormente

Estos conceptos están directamente relacionados con una de las configuraciones que asumimos como activadas al principio: los *flakes*.

Un *flake* es una ubicación que expone expresiones de Nix (el lenguaje de programación), que pueden usarse para construir o compilar paquetes, ejecutar aplicaciones, crear entornos de desarrollo (que exploraremos en el siguiente artículo) o describir sistemas completos del sistema operativo NixOS o macOS (más sobre esto en el futuro). Estas expresiones expuestas por un *flake* son sus **salidas**. Los *flakes* pueden **componerse**, lo que siginifica que un *flake* puede declarar otros *flakes* como dependencias, y las expresiones expuestas por tu *flake* pueden ser usadas por otros *flakes* como dependencias, etc.

Cuando digo ubicación, me refiero a un directorio que contiene tanto un fichero `flake.nix` como un `flake.lock`. La *referencia* de un *flake* es, por tanto, una indicación de cómo y dónde está esta ubicación. Hay varios tipos de referencias, dejo algunos ejemplos:

| Referencia                                                             | Descripción                                                                                             |
| ---------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------- |
| `path:/some/directory`                                                 | La ubicación `/some/directory` en el sistema                                                            |
| `github:DavSanchez/Nix-Relic`                                          | El repositorio de GitHub `DavSanchez/Nix-Relic`                                                         |
| `github:DavSanchez/Nix-Relic/feature/something`                        | La rama `feature/something` del repo de GitHub `Nix-Relic`                                              |
| `github:DavSanchez/Nix-Relic/d339a75f75ddc695b62e866ecdba2d23035d24af` | El *commit* `d339a75f75ddc695b62e866ecdba2d23035d24af` del repositorio de GitHub `DavSanchez/Nix-Relic` |
| `nixpkgs`                                                              | La revisión más reciente de la rama `nixpkgs-unstable` de Nixpkgs (alias de `github:NixOS/nixpkgs`)     |
| `nixpkgs/release-24.05`                                                | La rama `release-24.05` de Nixpkgs                                                                      |

Entonces, cuando hicimos `nix run "nixpkgs#cowsay"`, utilizamos la revisión más reciente de la rama `nixpkgs-unstable` del repositorio `NixOS/nixpkgs`, buscamos allí la expresión llamada `cowsay`, y la ejecutamos. Pero, como ya hemos mencionado, ¡Nix no solo sirve para ejecutar binarios arbitrarios! En las siguientes entradas analizaremos en detalle más funcionalidades de Nix, en particular, cómo configurar entornos de desarrollo para tus proyectos en cualquier lenguaje.

Mientras llega esa próxima entrada, puedes echar un vistazo a un *flake* que mantengo en GitHub, [Nix-Relic](https://github.com/DavSanchez/Nix-Relic), para ver ejemplos de [entradas](https://github.com/DavSanchez/Nix-Relic/blob/master/flake.nix#L4) y [salidas](https://github.com/DavSanchez/Nix-Relic/blob/master/flake.nix#L24-L30).

[^cowsay-example]: Este ejemplo está sacado de la [documentación de inicio rápido de Nix que mantiene DetSys](https://zero-to-nix.com/start/nix-run). Está en inglés, pero la recomiendo.
