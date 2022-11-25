module Registry.Effect.GitHub where

import Registry.App.Prelude

import Data.DateTime (DateTime)
import Foreign.GitHub (Address, IssueNumber, Tag)

data GitHubF a
  = ListTags Address (Array Tag -> a)
  | GetContent Address String FilePath (String -> a)
  | GetRefCommit Address String (String -> a)
  | GetCommitDate Address String (DateTime -> a)

data RegistryF a
  = ListPackagingTeam (Array String -> a)
  | CreateComment IssueNumber String a
  | CloseIssue IssueNumber a
  | CommitMetadata Metadata a
  | CommitManifest Manifest a
  | CommitPackageSet PackageSet a
