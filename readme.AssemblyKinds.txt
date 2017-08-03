Common assembly types and their descriptions
=================================================

== Drivers
Class Registrations
  Register the name, descriptions, property pages and other details for a given type of devices/drivers.
    Microsoft.Windows.Hardware.Devices.ClassInstaller.[CLASSNAME]-DriverClass
    c_[classname].inf 		(since W10)
    dual_c_[classname].inf 	(since W10)

Bus Drivers
  Enumerate Plug and Play devices connected to a given controller or a bus and pass their IDs to the system.

Device Drivers
  Service real devices or emulate virtual ones.


== Hardware
Function Discovery plugins
  Make devices from a certain source, with their functions, discoverable via Function Discovery.

Device Association Framework plugins [W10]
  Appear in the same places as Function Discovery plugins, so perhaps similar in role.

Print Providers
  Provide access to print spoolers (on this or other PC).

Print Monitors
  Provide access to various printer outputs for local print provider.


== Management, Events and Telemetry

WMI plugins
  Provide configurable options to be accessed through WMI.

Event instrumentation
  Defines potential events produced in ETW (event logs) by some component. May contain resource DLLs with message texts.

Performance counter providers / instrumentation

Windows Diagnostics Infrastructure
  WDI helpers

Problem reports and solutions helpers

Network Diagnostics Infrastructure

PerfTrack plugins
SQM plugins


== Servicing
Migration rules
  Define operations to perform to upgrade settings placed in older locations. Group with the component which is doing the upgrading, or with the migration framework (if the scope is broad or until the owner is found).

Sysprep Generalization/Cleanup providers
  Delete personal data, keys and settings in its respective area as a part of preparing the OS image for shipping to multiple customers.
  Usually a DLL with a neatly named functions which are registered in registry's sysprep key.

Windows System Reset Platform


== Administrative tools
MMC Snap-ins
  Snap-ins for the Microsoft Management Console. Put into MMC subfolder (for common general-purpose snap-ins) or with the owner component.

Group Policy Editor Snap-ins
  GPE MMC snap-in has its own snap-ins which are very similar to normal MMC snap-ins. Put into Group Policy editor component or with the owner component.

MDM Configuration Service Providers (CSP)
  Provide confiugrable options to be accessed through Mobile Device Management (MDM) interfaces. There's a MDM to WMI bridge WMI provider.

Group Policy definitions 
  -Adm

Control Panel plugins
  Either as *.cpl, a namespace DLL or a WinRT DLL.

Netsh plugins.
  -Netsh


== Other
Policy.1.0.Something.something
  .NET version redirection. Allows to define rules by which requested assemblies can be substituted by other assemblies. Group with the component which is doing the substituting.

