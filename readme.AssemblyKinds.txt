

==== Typical assembly types and their descriptions

Migration rules
  Define operations to perform to upgrade settings placed in older locations. Group with the component which is doing the upgrading, or with the migration framework (if the scope is broad or until the owner is found).

Policy.1.0.Something.something
  .NET version redirection. Allows to define rules by which requested assemblies can be substituted by other assemblies. Group with the component which is doing the substituting.

MMC Snap-ins
  Snap-ins for the Microsoft Management Console. Put into MMC subfolder (for common general-purpose snap-ins) or with the owner component.


==== A list of system facilities into which many components integrate:

- Control Panel, either as *.cpl, a namespace DLL or a  DLL
- Group Policy definitions (Adm)
- Netsh app (plugins)
- Windows Diagnostics Infrastructure
  - PerfTrack plugin
- SQM plugin
