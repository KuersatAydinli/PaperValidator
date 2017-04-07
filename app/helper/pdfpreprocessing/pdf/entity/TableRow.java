package helper.pdfpreprocessing.pdf.entity;

/**
 * Created by Aydinli on 04.04.2017.
 */
import java.util.ArrayList;
import java.util.List;
public class TableRow {

    //--------------------------------------------------------------------------
    //  Members
    private final int idx;
    private final List<TableCell> cells = new ArrayList<>();

    //--------------------------------------------------------------------------
    //  Initialization
    public TableRow(int idx) {
        this.idx = idx;
    }

    //--------------------------------------------------------------------------
    //  Getter N Setter
    public int getIdx() {
        return idx;
    }

    public List<TableCell> getCells() {
        return cells;
    }

    public boolean equals(TableRow r2){
//        Boolean equal = true;
//        for(int i=0; i<this.getCells().size();i++){
//            if(!this.getCells().get(i).getContent().equalsIgnoreCase(r2.getCells().get(i).getContent())){
//                equal = false;
//            }
//        }
//        return equal;
        StringBuilder sb1 = new StringBuilder();
        for(TableCell cellFirst : this.getCells()){
            sb1.append(cellFirst.getContent());
        }

        StringBuilder sb2 = new StringBuilder();
        for(TableCell cellSecond : r2.getCells()){
            sb2.append(cellSecond.getContent());
        }
        return sb1.toString().toLowerCase().replaceAll("\\s","").equalsIgnoreCase(sb2.toString().toLowerCase().replaceAll("\\s",""));
    }

    //--------------------------------------------------------------------------
    //  Method binding
    //--------------------------------------------------------------------------
    //  Implement N Override
    @Override
    public String toString() {
        StringBuilder retVal = new StringBuilder();
        int lastCellIdx = 0;
        for (TableCell cell : cells) {
            for (int idx2 = lastCellIdx; idx2 < cell.getIdx() - 1; idx2++) {
                retVal.append(";");
            }
            if (cell.getIdx() > 0) {
                retVal.append(";");
            }
            retVal.append(cell.getContent());
            lastCellIdx = cell.getIdx();
        }
        //return
        return retVal.toString();
    }
    //--------------------------------------------------------------------------
    //  Utils
    //--------------------------------------------------------------------------
    //  Inner class
}
