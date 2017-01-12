ManifestEnum: Assembly database inspector for Windows.

**Warning!!** This is a tool for advanced users. Some functions may break your system and void your warranty. Read below before using.

**Warning** This tool is still being developed, lots of things don't work, use on your own judgemtn.

Windows versions since XP are componentized in assemblies. Each assembly defines one file or primitive component, with all its registry settings and dependencies.

Windows contains a database of all installed assemblies, which is called SxS (from "Side-by-Side Assemblies").

This tool scans these assemblies, analyzes their contents and lets you inspect how your operating system is composed of these primitives. It inspects:

* Files
* Registry keys and values
* Services
* Scheduled Tasks
* Shell extensions

And more.

Have you ever wondered what does some service do, what is it needed for, why is it installed? Now you can figure it out.

Up to here the application is safe. It changes nothing on your PC and only inspects.

Additionally this tool supports:

* Unpacking SxS files (credit to Aunty Mel Package Extractor)
* Extracting assembly contents

**Warning! Alert!** The following functions are experimental. You *will* break your warranty and *may* break your PC. You **should not** use these on a PC where you have anything important stored.

* Uninstalling deployment assemblies (those that can be uninstalled with OS)
* Uninstalled ANY assemblies (by converting them to deployments)
* Installing deployment assemblies manually


### Usage

Run ManifestEnum and let the application build the database. It will take a long time at first, but subsequent runs will not rebuild it and if anything changes, you can update it from the main menu.

The interface presents you with a list of all assemblies, which you can filter, and a few 


### For a programmer

This tool contains a number of algorithms and info about SxS internals which are hard to find and you might perhaps find useful:

* Unpacking algorithms (credit to Aunty Mel) -- see `MSDeltaLib.pas` and `SxsExpand.pas`
* Keyform generation algorithms (keyforms are hash-like additions to names throughout SxS) -- see `SxsUtils.pas`
* SxS registry (COMPONENTS hive) internals partially explained -- see `SxsUtils.pas`
* Assembly management via IAssemblyCache (installing and uninstalling) -- see `WinSxS.pas`
* Fast SAX XML parser built on MSXML for speedy SxS manifest cache parsing

Useful parts are given extensive comments.

You can also just use the already built assembly database (SQLite3) for your needs.

Pascal programmers can even just do `TAssemblyDb.Create(filename)` and then call things like `Db.Assemblies.Get()`. The database code is in `AssemblyDb.*.pas`


### Componentization effort

This project also contains an effort to properly componentize modern version of Windows (7+) into a set of distinct applications, technologies and features which can be uninstalled without affecting other components, and explain their purpose and relations.

From the looks of it, CBS Components should have been used for this purpose, but Microsoft have instead decided to lump all OS features together into a few huge components where features are not listed separately.

And there's no way to override this grouping and introduce more nuanced packages with *those same* assemblies (long story short, assemblies have to be signed with the same key that signs their host CBS package).

For this reason, ManifestEnum introduces Bundles. A Bundle is a grouping of assemblies similar to a Component in CBS, but detached from it. A Bundle is defined by a Bundle text file which lists all assemblies contained in it (and in the future perhaps CBS Components) and perhaps comments explaining their purpose.

ManifestEnum scans Bundle structure in the BundleData folder next to the application and displays the assemblies grouped like that.

An effort is being made to group existing assemblies into bundles and categorize bundles according to their purpose, so that the whole operating system is properly understood.

It is not clear yet whether this is a realistic task and whether the results will be useful, so the bundles are not included by default in the release folder.

To enable bundles, copy or junction BundleData from the project root to the release folder, but do not commit it.



### Requirements
Requires SQLite3.dll in the application folder.


### Project status
This project is still in initial development. Many things are not polished or not done at all.

