

C:\Windows\winsxs
  Содержит подпапку для каждой assembly, в которой лежат все относящиеся к ней файлы.

C:\Windows\winsxs\Manifests
  Содержит манифесты для всех assemblies.
  Windows 7: В открытом виде.

HKEY_LOCAL_MACHINE\Software\Microsoft\Windows\CurrentVersion\Component Based Servicing\Packages
  Перечисляет все пакеты в системе. Для каждого указано название mum-файла в InstallName

C:\Windows\servicing\Packages
  Содержит mum и cat-файлы для каждого пакета.

В mum-файле находятся:
  assembly (с assemblyIdentity), в ней update, в ней один или несколько component, который ссылается на assemblyIdentity компонента.


Чем отличаются пакеты от assemblies?
И те, и другие содержат:
  <assembly>
    <assemblyIdentity ... />
    <!-- contents -->
    ...
  </assembly>
Различается contents.

В пакете лежит:
  <package>
    <parent><assemblyIdentity /></parent>
    <update>
      <component><assemblyIdentity /></component>
    </update>
  </package>

В assembly лежит:
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