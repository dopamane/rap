module Data.Rap (Rap, Stanza(..)) where

type Rap = [Stanza]

data Stanza = Stanza deriving (Eq, Read, Show)
