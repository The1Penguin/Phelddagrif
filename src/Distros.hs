module Distros where

data Distro where
  Ubuntu :: Distro
  Mint :: Distro
  PopOS :: Distro
  Arch :: Distro
  NixOS :: Distro
  Fedora :: FedoraSpins -> Distro
  CachyOS :: Distro
  Debian :: Distro
  OpenSuse :: SuseSpins -> Distro
  Elementary :: Distro
  Artix :: Distro
  PuppyLinux :: Distro
  Alpine :: Distro
  Gentoo :: Distro
  Hannah_Montana :: Distro
  LFS :: Distro
  QubeOS :: Distro
  Nyarch :: Distro
  Uwuntu :: Distro
  YiffOS :: Distro
  Void :: Distro
  Zorin :: Distro
  Tails :: Distro

data FedoraSpins where
  WorkStation :: FedoraSpins
  KDE_Plasma :: FedoraSpins
  Silverblue :: FedoraSpins
  Kinoite :: FedoraSpins

data SuseSpins where
  Tumbleweed :: SuseSpins
  Leap :: SuseSpins

all, beginner, immutable, de, bleeding_edge, stable, windows_like, minimal, memes, security :: [Distro]
all = [ Ubuntu
      , Mint
      , PopOS
      , Arch
      , NixOS
      , Fedora WorkStation
      , Fedora KDE_Plasma
      , Fedora Silverblue
      , Fedora Kinoite
      , CachyOS
      , Debian
      , OpenSuse Tumbleweed
      , OpenSuse Leap
      , Elementary
      , Artix
      , PuppyLinux
      , Alpine
      , Gentoo
      , Hannah_Montana
      , LFS
      , QubeOS
      , Nyarch
      , YiffOS
      , Void
      , Zorin
      , Tails
      ]

beginner = [ Ubuntu
           , Mint
           , PopOS
           , NixOS
           , Fedora WorkStation
           , Fedora KDE_Plasma
           , CachyOS
           ]
immutable = [ NixOS
            , Fedora Silverblue
            , Fedora Kinoite]
de = [ Ubuntu
     , Mint
     , PopOS
     , Fedora WorkStation
     , Fedora KDE_Plasma
     , Fedora Silverblue
     , Fedora Kinoite
     , CachyOS
     , Debian
     , OpenSuse Tumbleweed
     , OpenSuse Leap
     , Elementary
     , Hannah_Montana
     , Nyarch
     , Zorin
     , Tails
     , PuppyLinux]
windows_like = [ Mint
               , Fedora KDE_Plasma
               , Fedora Kinoite
               , CachyOS
               , OpenSuse Tumbleweed
               , OpenSuse Leap
               , Hannah_Montana
               , Zorin
               , PuppyLinux]
minimal = [ NixOS
          , Arch
          , Artix
          , Alpine
          , Gentoo
          , QubeOS
          , LFS
          , Void
          , YiffOS]
memes = [ Arch
        , NixOS
        , Artix
        , Alpine
        , Gentoo
        , Hannah_Montana
        , LFS
        , QubeOS
        , Nyarch
        , Uwuntu
        , YiffOS]
security = [ QubeOS
           , Tails
           , LFS]
bleeding_edge = [ OpenSuse Tumbleweed
                , Arch
                , NixOS
                , CachyOS
                , Artix
                , Gentoo
                , LFS
                , QubeOS
                , Nyarch
                , Void]
stable = [ OpenSuse Leap
         , Ubuntu
         , Mint
         , PopOS
         , NixOS
         , Fedora WorkStation
         , Fedora KDE_Plasma
         , Fedora Silverblue
         , Fedora Kinoite
         , Debian
         , Hannah_Montana
         , Tails]
