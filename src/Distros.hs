{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Distros where

import Data.Functor
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.State
import Text.Printf
import Rando (pickOne)
import Foreign.C.Error (eHOSTDOWN)

data Tree a where
  Node :: a -> Tree a -> Tree a -> Tree a
  Leaf :: Tree a

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
  deriving Eq

instance Show Distro where
  show = \case
            Ubuntu -> "Ubuntu"
            Mint -> "Linux Mint"
            PopOS -> "Pop!_OS"
            Arch -> "Arch Linux"
            NixOS -> "NixOS"
            Fedora WorkStation -> "Fedora Workstation"
            Fedora KDE_Plasma -> "Fedora KDE Plasma"
            Fedora Silverblue -> "Fedora Silverblue"
            Fedora Kinoite -> "Fedora kinoite"
            CachyOS -> "CachyOS"
            Debian -> "Debian"
            OpenSuse Tumbleweed -> "OpenSUSE Tumbleweed"
            OpenSuse Leap -> "OpenSUSE Leap"
            Elementary -> "elementary OS"
            Artix -> "Artix Linux"
            PuppyLinux -> "Puppy Linux"
            Alpine -> "Alpine Linux"
            Gentoo -> "Gentoo"
            Hannah_Montana -> "Hannah Montana Linux"
            LFS -> "Linux from scratch"
            QubeOS -> "QubeOS"
            Nyarch -> "Nyarch"
            YiffOS -> "YiffOS"
            Void -> "Void Linux"
            Zorin -> "Zorin OS"
            Tails -> "Tails"

data FedoraSpins where
  WorkStation :: FedoraSpins
  KDE_Plasma :: FedoraSpins
  Silverblue :: FedoraSpins
  Kinoite :: FedoraSpins
  deriving Eq

data SuseSpins where
  Tumbleweed :: SuseSpins
  Leap :: SuseSpins
  deriving Eq

all_distros, beginner, immutable, de, bleeding_edge, stable, windows_like, minimal, memes, security :: [Distro]
all_distros = [ Ubuntu
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
            , Fedora Kinoite
            ]
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
     , PuppyLinux
     ]

windows_like = [ Mint
               , Fedora KDE_Plasma
               , Fedora Kinoite
               , CachyOS
               , OpenSuse Tumbleweed
               , OpenSuse Leap
               , Hannah_Montana
               , Zorin
               , PuppyLinux
               ]

minimal = [ NixOS
          , Arch
          , Artix
          , Alpine
          , Gentoo
          , QubeOS
          , LFS
          , Void
          , YiffOS
          ]

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
        , YiffOS
        ]

security = [ QubeOS
           , Tails
           , LFS
           ]

bleeding_edge = [ OpenSuse Tumbleweed
                , Arch
                , NixOS
                , CachyOS
                , Artix
                , Gentoo
                , LFS
                , QubeOS
                , Nyarch
                , Void
                ]

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
         , Tails
         ]

simp :: Eq a => Bool -> [a] -> a -> Bool
simp = \case
  True -> flip elem
  False -> (not .) . flip elem

type Question = (Text, Bool -> [Distro] -> [Distro])

memes_question :: Question
memes_question = ("Do you want a memeable distro?", filter . (`simp` memes))

beginner_question :: Question
beginner_question = ("Would you classify yourself as a beginner?", filter . (`simp` beginner))

immutable_question :: Question
immutable_question = ("Do you want a immutable filesystem?", filter . (`simp` immutable))

display_question :: Question
display_question = ("Do you want a display manager installed by default?", filter . (`simp` de))

windows_question :: Question
windows_question = ("Do you want a said display manager to look like Windows?", filter . (`simp` windows_like))

minimal_question :: Question
minimal_question = ("Do you want a minimal system?", filter . (`simp` minimal))

security_question :: Question
security_question = ("Is a security focused distro of importance?", filter . (`simp` security))

bleeding_edge_question :: Question
bleeding_edge_question = ("Do you want your packages to be bleeding edge?", filter . (`simp` bleeding_edge))

stable_question :: Question
stable_question = ("Do you want your packages to be stable?", filter . (`simp` stable))

question_tree :: Tree Question
question_tree = Node beginner_question display_tree display_tree
  where
    display_tree = Node display_question minimal_tree windows_tree
    minimal_tree = Node minimal_question immutable_tree immutable_tree
    windows_tree = Node windows_question immutable_tree immutable_tree
    immutable_tree = Node immutable_question stable_tree stable_tree
    stable_tree = Node stable_question bleeding_edge_tree security_tree
    bleeding_edge_tree = Node bleeding_edge_question security_tree security_tree
    security_tree = Node security_question memes_tree Leaf
    memes_tree = Node memes_question Leaf Leaf

runQuiz :: IO ()
runQuiz = execStateT (execTree question_tree) all_distros >>= \case
                [] -> pure NixOS
                xs -> pickOne xs
          >>= printf "You should choose %s" . T.show
  where
    execTree :: Tree Question -> StateT [Distro] IO ()
    execTree = \case
      Leaf -> pure ()
      Node q l r -> pure ()
