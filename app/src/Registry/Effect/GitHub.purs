module Registry.Effect.GitHub where

import Registry.App.Prelude

import Data.DateTime (DateTime)
import Foreign.GitHub (Address, IssueNumber, Tag, Team)

data GitHub a
  = ListTags Address (Array Tag -> a)
  | ListTeamMembers Team (Array String -> a)
  | GetContent Address String FilePath (String -> a)
  | GetRefCommit Address String (String -> a)
  | GetCommitDate Address String (DateTime -> a)

data AuthGitHub a
  = CloseIssue IssueNumber a
  | CommitMetadata Metadata a
  | CommitManifest Manifest a
  | CommitPackageSet PackageSet a
