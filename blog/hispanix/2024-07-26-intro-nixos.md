---
title: "Introducción a NixOS"
date: 2024-07-26
---
<!-- LTeX: language=es -->

![](./nixos.png)

[NixOS](https://nixos.org) es el sistema operativo basado en el gestor de paquetes Nix del que estamos hablando en estos últimamente en este blog. Lo que ofrece respecto a otras distribuciones es una extensión de lo que ya propone Nix: configuración y despliegues declarativos y reproducibles.

Cuenta con un poderoso sistema de módulos que permiten configurar la práctica totalidad del sistema operativo (*bootloader*, sistema de archivos, gestores de ventanas, módulos del *kernel*, servicios de `systemd` como OpenSSH, etc) desde algo tan simple como un único fichero en lenguaje Nix. Cuenta con [muchas opciones](https://search.nixos.org/options) definidas para ello.

En NixOS puedes realizar cambios en esta configuración de forma atómica. Básicamente, es muy difícil que el sistema quede en un estado intermedio tras aplicar un cambio en la configuración, cosa que es más sencillo que ocurra en sistemas que usan la estructura de directorios clásica de Linux, conocida como [FHS](https://en.wikipedia.org/wiki/Filesystem_Hierarchy_Standard). Gracias a esto, puedes alternar entre configuraciones que hayas creado con éxito mediante su sistema de generaciones, lo que permite hacer fácilmente un *rollback* si una configuración que aplicas no tiene los resultados que esperabas.

Muchas de las otras cosas que se hacen posibles gracias a Nix están disponibles en NixOS. Por ejemplo, es posible que cada usuario que esté definido en un sistema que ejecuta NixOS use una versión diferente del mismo programa, ya que ambas versiones pueden coexistir al habitar en distintas rutas de la Nix Store.

Este es el aspecto que tiene la configuración de un sistema NixOS:

```nix
# Como es de esperar, una configuración de NixOS es una función que recibe, entre otras cosas, un parámetro con una colección de paquetes determinada
{ pkgs, ... }: 
{
  # Activa Nix flakes y la CLI unificada (ver los artículos anteriores)
  nix.settings = {
    experimental-features = "nix-command flakes";
  };

  # Muchas configuraciones son evidentes
  networking.hostName = "NixOS-VM";

  # Para poder conectarte a la máquina por SSH
  services.openssh.enable = true;

  # Sistema de ficheros raíz
  fileSystems."/" = {
    device = "/dev/sda1";
    fsType = "ext4";
  };

  # Un usuario definido
  users.users.david = {
    isNormalUser = true;
    # Existen opciones para gestionar las contraseñas
    # ¡No sería seguro incluirlas tal cual aquí!
    # Acabarían en texto plano en la Nix Store
  };

  # Paquetes definidos a nivel de sistema
  environment.systemPackages = with pkgs; [
    curl
    jq
    wget
    git
    python
    openssl
    zsh
  ];
}
```

Puedes explorar todas las opciones disponibles, sus valores por defecto (si los tienen) y una breve documentación en [el buscador oficial](https://search.nixos.org/options).

En los siguientes artículos veremos qué cosas pueden hacerse con NixOS. Si quieres experimentar con este sistema operativo, en su [web oficial](https://nixos.org/download/#) ofrecen imágenes usables como *Live CDs* con entornos de escritorio GNOME o KDE Plasma, ISOs mínimas para instalaciones en servidores, máquinas virtuales listas para usar con VirtualBox o AMIs de AWS para desplegar en EC2.

En los *Live CDs* puedes instalar NixOS de forma gráfica al igual que en otras distribuciones como Ubuntu o similares. En el [manual de NixOS](https://nixos.org/manual/nixos/stable/) tienes más opciones para una instalación a mano.
