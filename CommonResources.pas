unit CommonResources;

interface

uses
  System.SysUtils, System.Classes, Vcl.ImgList, Vcl.Controls;

type
  TResourceModule = class(TDataModule)
    SmallImages: TImageList;
  end;

var
  ResourceModule: TResourceModule;

//Image Indices to image lists
const
  imgFolder = 0;
  imgFile = 1;
  imgRegistryValue = 2;
  imgTask = 3;
  imgAssembly = 4;
  imgCategory = 5;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
