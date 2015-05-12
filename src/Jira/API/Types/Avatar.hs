{-# LANGUAGE TupleSections #-}

module Jira.API.Types.Avatar ( AvatarSize(..)
                             , AvatarUrls(..)
                             , avatarSized
                             , largeAvatar
                             , smallAvatar
                             ) where

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Maybe (catMaybes)
import Data.String.Conversions
import qualified Data.Map.Strict as Map

data AvatarSize = AvatarSize16 | AvatarSize24 | AvatarSize32 | AvatarSize48
                deriving (Eq, Ord, Enum, Bounded)

instance Show AvatarSize where
  show AvatarSize16 = "16x16"
  show AvatarSize24 = "24x24"
  show AvatarSize32 = "32x32"
  show AvatarSize48 = "48x48"

newtype AvatarUrls = AvatarUrls { unAvatarUrls :: Map.Map AvatarSize String
                                } deriving (Show, Eq)

instance FromJSON AvatarUrls where
  parseJSON = withObject "Expected object" $ \o ->
    AvatarUrls . Map.fromList . catMaybes <$> mapM (makePair o) [AvatarSize16 ..]
    where makePair o size = o .:? cs (show size) & mapped.mapped %~ (size,)

avatarSized :: AvatarSize -> AvatarUrls -> Maybe String
avatarSized size = Map.lookup size . unAvatarUrls

largeAvatar :: AvatarUrls -> Maybe String
largeAvatar = pickWith (lastOf traverse)

smallAvatar :: AvatarUrls -> Maybe String
smallAvatar = pickWith (firstOf traverse)

pickWith :: ([AvatarSize] -> Maybe AvatarSize) -> AvatarUrls -> Maybe String
pickWith f (AvatarUrls urls) = (f . Map.keys) urls >>= flip Map.lookup urls
