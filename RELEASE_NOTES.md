* 0.8.5 - Fix redundant downloads to AssemblyCache issue.
* 0.8.4 - Update FsPickler to latest version.
* 0.8.3 - Fix concurrency issue in AssemblyCache.
* 0.8.2 - Bugfixes in caching mechanism. Corrections in the uploader/downloader API.
* 0.8.1 - Expose Vagabond.TryParseAssemblySliceName method to public API.
* 0.8.0 - Use global, hash-based filenames for persisted data dependencies.
* 0.7.9 - Dependency resolution bugfix.
* 0.7.8 - Dependency resolution bugfix.
* 0.7.7 - Fix cyclic dependency issue in Microsoft assemblies.
* 0.7.6 - Fix bug related to F# 4.0 quotation literals.
* 0.7.5 - Use amortized initialization in AppDomain pool.
* 0.7.4 - Use sequential, asynchronous initialization in AppDomain pool.
* 0.7.3 - Add GZipStream compression implementation.
* 0.7.2 - Revert changes to AppDomain pool.
* 0.7.1 - Expose data compression abstractions; Tie AppDomain pool API to Vagabond.
* 0.7.0 - Refinements to the API, changes in the Assembly loading logic.
* 0.6.12 - Changes to assembly loading logic.
* 0.6.11 - Initialize AppDomain pool in parallel.
* 0.6.10 - Improve Assembly import logic.
* 0.6.9 - Fix bug in AppDomain evaluator asynchronous operations.
* 0.6.8 - Update dependency traversal logic using FsPickler visitor re-implementation.
* 0.6.7 - Update FsPickler to its latest version.
* 0.6.6 - Expose static binding information in VagabondManager.
* 0.6.5 - Expose hashcodes for pickled static properties.
* 0.6.4 - Improve quotation literal support; improve static field support.
* 0.6.3 - Improve cyclic dependency error reporting.
* 0.6.2 - Ignored assemblies bugfix.
* 0.6.1 - Misc bugfixes.
* 0.6.0 - Rewrite data exportation/importation logic and API.
* 0.5.4 - Bugfixes in the native assembly implementation.
* 0.5.3 - Use shorter file names in assembly cache.
* 0.5.2 - Implement Vagabond.GetFileName for assembly ids.
* 0.5.1 - Improve assembly exportation API, add support for unmanaged assemblies.
* 0.5.0 - Refactor API, pickle vagabond data to files, add compression, bug fixes.
* 0.3.2 - Update AppDomainPool, add examples and tests.
* 0.3.1 - Implement AppDomainPool.
* 0.3.0 - Rename to Vagabond, improve exception messages in topological sorting.
* 0.2.9 - fix API issue.
* 0.2.8 - add ignored assemblies functionality in Vagabond initialization API.
* 0.2.7 - rename VagabondAssembly type, add support for user-defined assembly ignore rules
* 0.2.6 - bug fixes
* 0.2.5 - update FsPickler
* 0.2.4 - update dependency traversal logic
* 0.2.3 - fix packaging issues
* 0.2.2 - update FsPickler
* 0.2.1 - update FsPickler
* 0.2.0 - update FsPickler, change dependency resolution method, simplify public API
* 0.1.4 - bug fix in transitive dependency resolution
* 0.1.3 - bug fix in dependency traversal
* 0.1.2 - add .IsLoadedAssembly method
* 0.1.1 - add AssemblyCache utility, add support for assembly hashes
* 0.1.0 - initial release