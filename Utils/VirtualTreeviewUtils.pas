unit VirtualTreeviewUtils;

interface
uses VirtualTrees;

type
  TVirtualTreeviewHelper = class helper for TVirtualStringTree
  protected
    procedure SetNodeVisibleCallback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer;
      var Abort: Boolean);
  public
    procedure MakeNodePathVisible(Node: PVirtualNode);
    procedure MakeNodeContentVisible(Node: PVirtualNode);
  end;

implementation

//Makes node and all of its parents to be visible
//Assumes you're calling this uniformly, so if a parent is already visible then no further
//ascending is needed
procedure TVirtualTreeviewHelper.MakeNodePathVisible(Node: PVirtualNode);
begin
  repeat
    Self.IsVisible[Node] := true;
    Node := Self.NodeParent[Node];
  until (Node = nil) or Self.IsVisible[Node];
end;

//Makes node, all of its parents and all of its children visible
procedure TVirtualTreeviewHelper.MakeNodeContentVisible(Node: PVirtualNode);
begin
  //Can't optimize this away by checking if already visible: not all children may be visible
  MakeNodePathVisible(Node);
  Self.IterateSubtree(Node, Self.SetNodeVisibleCallback, pointer(true));
end;

procedure TVirtualTreeviewHelper.SetNodeVisibleCallback(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Data: Pointer; var Abort: Boolean);
begin
  Self.IsVisible[Node] := boolean(Data);
end;


end.
