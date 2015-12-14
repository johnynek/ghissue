# ghissue
Command-line Github issues client. This is super early, and just posting for help from friends.

## Running
```
stack build
stack exec ghissue-exe show
```

## Configuring

This is currently done by having a file ".ghissue.config" in the current directory
with one line of the format:
```
oauthstring githuborg repo
```
To get an oauth string, go to https://github.com/settings/tokens and create a new token. Copy the
auth string and save it into that file.

## Usage

### show

`ghissues show 90-200` pointing at the `twitter/summingbird` repo might return:

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
