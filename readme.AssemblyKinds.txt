

==== Typical assembly types and their descriptions

MMC Snap-ins
  Snap-ins for the Microsoft Management Console. Put into MMC subfolder (for common general-purpose snap-ins) or with the owner component.

Group Policy Editor Snap-ins
  GPE MMC snap-in has its own snap-ins which are very similar to normal MMC snap-ins. Put into Group Policy editor component or with the owner component.

WMI plugins
  Provide configurable options to be accessed through WMI.

MDM Configuration Service Providers (CSP)
  Provide confiugrable options to be accessed through Mobile Device Management (MDM) interfaces. There's a MDM to WMI bridge WMI provider.

Event instrumentation
  Defines potential events produced in ETW (event logs) by some component. May contain resource DLLs with message texts.

Migration rules
  Define operations to perform to upgrade settings placed in older locations. Group with the component which is doing the upgrading, or with the migration framework (if the scope is broad or until the owner is found).

Policy.1.0.Something.something
  .NET version redirection. Allows to define rules by which requested assemblies can be substituted by other assemblies. Group with the component which is doing the substituting.

Print Providers
  Provide access to print spoolers (on this or other PC).

Print Monitors
  Provide access to various printer outputs for local print provider.

Sysprep Generalization/Cleanup providers
  Delete personal data, keys and settings in its respective area as a part of preparing the OS image for shipping to multiple customers.
  Usually a DLL with a neatly named functions which are registered in registry's sysprep key.

Windows System Reset Platform

==== A list of system facilities into which many components integrate:

- Control Panel, either as *.cpl, a namespace DLL or a  DLL
- Group Policy definitions (Adm)
- Netsh app (plugins)
- Windows Diagnostics Infrastructure
  - PerfTrack plugin
- SQM plugin
