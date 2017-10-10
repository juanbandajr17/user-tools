public class ActorGraphNode {
    private String name;
    private Set<ActoGraphNode> linkedActors;
    private int baconNumber = -1;

    public ActorGraphNode(String name) {
        this.name = name;
        linkedActors = new HashSet<ActorGraphNode>();
    }

    public void linkCostar(ActorGraphNode costar) {
        linkedActors.add(costar);
        costar.linkedActors.add(this);
    }

    public int getBaconNumber() { return baconNumber; }

    public void setBaconNumbers() {
        baconNumber = 0;
        Queue<ActorGraphNode> queue = new LinkedList<ActorGraphNode>();
        queue.add(this);
        ActorGraphNode current;

        while((current = queue.poll()) != null) {
            for(ActorGraphNode n : current.linkedActors) {
                if(n.baconNumber == -1) { // unvisited
                    n.baconNumber = current.baconNumber + 1;
                    queue.add(n);
                }
            }
        }
    }
}
