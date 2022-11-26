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

-- TODO: Creating a comment is really something LOG should do, so it should
-- depend on the GITHUB effect when interpreting in that environment. But it
-- and everything else should use LOG as an effect...so they rely on each other.
--
-- How to handle this?
data AuthGitHub a
  = CreateComment IssueNumber String a
  | CloseIssue IssueNumber a
  | CommitMetadata Metadata a
  | CommitManifest Manifest a
  | CommitPackageSet PackageSet a
