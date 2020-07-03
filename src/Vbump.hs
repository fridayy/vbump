module Vbump 
    (parseArgs, bump, latestVersion, ReleaseVersion (..))
where

import Text.Read
import Data.Maybe
import Data.List
import Data.List.Split

parseArgs :: [String] -> String
parseArgs ["major"] = "major"
parseArgs ["minor"] = "minor"
parseArgs ["patch"] = "patch"
parseArgs ["-h"] = "This is some cool help text"
parseArgs (_:_) = "Unknown arguments supplied. Use -h to get some help"
parseArgs [] = ""

data ReleaseVersion = ReleaseVersion {
    major :: Int,
    minor :: Int,
    patch :: Int
} deriving (Show, Eq, Ord)

sortDesc x = reverse $ sort x

-- safe head
maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:xs) = Just x

-- bumps the given version 
bump :: String -> Maybe ReleaseVersion -> String
bump "major" Nothing = "1.0.0"
bump "minor" Nothing = "0.2.0"
bump "patch" Nothing = "0.1.1"
bump "major" (Just version) = toVersionString $ bumpVersion "major" version
bump "minor" (Just version) = toVersionString $ bumpVersion "minor" version
bump "patch" (Just version) = toVersionString $ bumpVersion "patch" version

-- returns the next ReleaseVersion
bumpVersion :: String -> ReleaseVersion -> ReleaseVersion
bumpVersion "major" version = ReleaseVersion { major = (major $ version) + 1, minor = 0, patch = 0 }
bumpVersion "minor" version = ReleaseVersion { major = (major $ version), minor = (minor $ version) + 1, patch = 0 }
bumpVersion "patch" version = ReleaseVersion { major = (major $ version), minor = (minor $ version), patch = (patch $ version) + 1 }
bumpVersion _ version = version

latestVersion :: [String] -> Maybe ReleaseVersion
latestVersion versions = maybeHead $ sortDesc $ parseVersions versions

toVersionString :: ReleaseVersion -> String
toVersionString version = (show $ major $ version) ++ "." ++ (show $ minor $ version) ++ "." ++ (show $ patch $ version)

parseVersions :: [String] -> [ReleaseVersion]
parseVersions versions = catMaybes $ map parseVersion versions

parseVersion :: String -> Maybe ReleaseVersion
parseVersion s = case split of
    [('v':major), minor, patch] -> toVersion $ map toReleaseVersionNumber [major, minor, patch] 
    [_, _, _] -> toVersion $ map toReleaseVersionNumber split 
    otherwise -> Nothing
    where split = splitOn "." s

toVersion :: [Maybe Int] -> Maybe ReleaseVersion
toVersion [(Just major), (Just minor), (Just patch)] = Just ReleaseVersion {
    major = major,
    minor = minor,
    patch = patch
}
toVersion _ = Nothing

toReleaseVersionNumber :: (String) -> Maybe Int
toReleaseVersionNumber n = readMaybe n :: Maybe Int