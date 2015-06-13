package corpnet.layout;

import org.abego.treelayout.NodeExtentProvider;

public class NetworkNodeExtentProvider implements NodeExtentProvider<LayoutNode> {

    private double width;
    private double height;

    public NetworkNodeExtentProvider(double width, double height) {
        this.width = width;
        this.height = height;
    }

    @Override
    public double getWidth(LayoutNode layoutNode) {
        return width;
    }

    @Override
    public double getHeight(LayoutNode layoutNode) {
        return height;
    }
}