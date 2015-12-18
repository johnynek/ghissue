# ghissue
Command-line Github issues client.

## Running
You need to [install stack](http://docs.haskellstack.org/en/stable/README.html#how-to-install) the
haskell build tool.
```
stack build
stack install
```
This will install `ghissue-exe` into your `~/.local/bin` which should be in your path after stack is
installed.

You can run without installing using stack:
```
stack exec ghissue-exe command -- <non-stack options after the -->
```

```
ghissue: a command line github issues tool

Usage: ghissue-exe COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  show                     Show issues
  new                      Create a new issue. If no --body is given stdin is
                           used.
  close                    Close one or more issues
  label                    Label one or more issues
  visit                    Visit an issue in your default web browser
  comment                  Attach a comment to a given issue. If no --body is
                           given stdin is used
  assign                   Assign one or more issues to a given user
```

## Configuring

This is currently done by having a file "ghissue.conf" in the `.git` directory for the
current project which contains one line of the format:
```
oauthstring githuborg repo
```
To get an oauth string, go to https://github.com/settings/tokens and create a new token. Copy the
auth string and save it into that file.

## Usage
`ghissue-exe --help` should give you help after you set up the configuration file. Each
command has its own help, e.g. `ghissue-exe show --help`.

### show

`ghissue-exe show 90-200` pointing at the `twitter/summingbird` repo might return:

```
190	Add regression tests for BatchID off-by-one bugs discovered.	
189	Add a storm-trident backend	
188	Add algebird.ExclusiveUpper[T] and algebird.InclusiveUpper[T] types to deal with off-by-one	cleanup, scalding
183	Investigate CRDTs	
155	Clean buffer join API	documentation, enhancement, storm
154	Transactional Storm Planner	enhancement
142	Add ability to create test Sources from scalacheck.Gen	
140	Tiered Aggregation for Scalding	enhancement, scalding
136	Treat Configuration as a mutable object, not immutable	enhancement, scalding
134	Implement WritableSink with template tap.	enhancement, scalding
112	ClientStore should take a "most recent batch" fn	client, enhancement
108	Implement code for Scalding sources to report maximum available DateRange	scalding
95	remove versionToBatchIDCompat	cleanup, scalding
94	Coordinate Metadata and Data retrieval in VersionedBatchStore	scalding
93	Move HDFSMetadata to scalding-commons	newbie, scalding
92	Use TemplateTap to write distinct batches	scalding
91	Don't Load unnecessary data in BatchedLookup	scalding
```

### new
Create a new issue

### visit
Visit an issue in the web browser (currently only works if you have a shell command called `open` in
path that opens your browser, as OSX would do).

### close
Close an existing issue

### label
Attach labels to issues
