Typical problems:
1. Assemblies are uninstalled very quickly but are still there after reboot.
  Enable "Options -> Force uninstall". Otherwise uninstall will work only for deployment assemblies (blue).

2. Assemblies are uninstalled, but remain in the list.
  Do dism /online /cleanup-image /startcomponentcleanup 
  then refresh the database (it's not done automatically after the uninstall anyway).


C:\Windows\winsxs
  Contains a subfolder for each assembly with all assembly files
  
C:\Windows\winsxs\Manifests
  Contains manifests for all assemblies.
  Windows 7: In plain text.
  Windows 10: In packed form.

HKEY_LOCAL_MACHINE\Software\Microsoft\Windows\CurrentVersion\Component Based Servicing\Packages
  Lists all installed CBS components. For each, InstallName contains it's .mum file name.

C:\Windows\servicing\Packages
  Contains .mum and .cat files for each installed component.


How are component manifests (*.mum) different from assembly ones (*.manifest)?
Both contain:
  <assembly>
    <assemblyIdentity ... />
    <!-- contents -->
    ...
  </assembly>
Contents part is different.

Component contains:
  <package>
    <parent><assemblyIdentity /></parent>
    <update>
      <component><assemblyIdentity /></component>
    </update>
  </package>

Assembly contains:
  <dependency><dependentAssembly><assemblyIdentity /></dependentAssembly></dependency>
  <file name="csc.sys">...</file>
  <directories>
    <directory ... />
  </directories>
  <memberships>
    <categoryMembership>...</categoryMembership>
    <categoryMembership>...</categoryMembership>
  </memberships>
  <trustInfo>...</trustInfo>
  <localization>...</localization>
  <configuration>...</configuration>