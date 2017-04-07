package helper.pdfpreprocessing.pdf.entity;

/**
 * Created by Aydinli on 04.04.2017.
 */
import java.util.ArrayList;
import java.util.List;

public class Table {

    //--------------------------------------------------------------------------
    //  Members
    private final int pageIdx;
    private final List<TableRow> rows = new ArrayList<>();
    private final int columnsCount;

    //--------------------------------------------------------------------------
    //  Initialization and releasation
    public Table(int idx, int columnsCount) {
        this.pageIdx = idx;
        this.columnsCount = columnsCount;
    }

    public Table(int columnsCount){
        this.columnsCount = columnsCount;
        this.pageIdx = 0;
    }

    //--------------------------------------------------------------------------
    //  Getter N Setter
    public int getPageIdx() {
        return pageIdx;
    }

    public int getColumnsCount(){
        return columnsCount;
    }

    public List<TableRow> getRows() {
        return rows;
    }

    public void addRows(List<TableRow> rows){
        this.rows.addAll(rows);
    }

    public void addRow(TableRow row){
        this.rows.add(row);
    }

    // filter duplicate rows in table
    public Table filterDuplicateRows(Table table){
        Table retTable = new Table(table.getColumnsCount());
        for(TableRow row : table.getRows()){
            if(!retTable.containsRow(retTable,row)){
                retTable.addRow(row);
            }
        }
        return retTable;
    }

    public boolean containsRow(Table table, TableRow row){
        Boolean contains = false;
        for(TableRow currRow : table.getRows()){
            if(currRow.equals(row)){
                contains = true;
                break;
            }
        }
        return contains;
    }

    public String toHtml() {
        return toString(true);
    }

    //--------------------------------------------------------------------------
    //  Method binding
    //--------------------------------------------------------------------------
    //  Implement N Override
    @Override
    public String toString() {
        return toString(false);
    }

    //--------------------------------------------------------------------------
    //  Utils
    private String toString(boolean inHtmlFormat) {
        StringBuilder retVal = new StringBuilder();
        if (inHtmlFormat) {
            retVal.append("<!DOCTYPE html>"
                    + "<html>"
                    + "<head>"
                    + "<meta charset='utf-8'>")
                    .append("</head>")
                    .append("<body>");
            retVal.append("<table border='1'>");
        }
        for (TableRow row : rows) {
            if (inHtmlFormat) {
                retVal.append("<tr>");
            } else if (retVal.length() > 0) {
                retVal.append("\n");
            }
            int cellIdx = 0;//pointer of row.cells
            int columnIdx = 0;//pointer of columns
            while (columnIdx < columnsCount) {
                if (cellIdx < row.getCells().size()) {
                    TableCell cell = row.getCells().get(cellIdx);
                    if (cell.getIdx() == columnIdx) {
                        if (inHtmlFormat) {
                            retVal.append("<td>")
                                    .append(cell.getContent())
                                    .append("</td>");
                        } else {
                            if (cell.getIdx() != 0) {
                                retVal.append(";");
                            }
                            retVal.append(cell.getContent());
                        }
                        cellIdx++;
                        columnIdx++;
                    } else if (columnIdx < cellIdx) {
                        if (inHtmlFormat) {
                            retVal.append("<td>")
                                    .append("</td>");
                        } else if (columnIdx != 0) {
                            retVal.append(";");
                        }
                        columnIdx++;
                    } else {
                        throw new RuntimeException("Invalid state");
                    }
                } else {
                    if (inHtmlFormat) {
                        retVal.append("<td>")
                                .append("</td>");
                    } else if (columnIdx != 0) {
                        retVal.append(";");
                    }
                    columnIdx++;
                }

            }
            if (inHtmlFormat) {
                retVal.append("</tr>");
            }
        }
        if (inHtmlFormat) {
            retVal.append(
                    "</table>")
                    .append("</body>")
                    .append("</html>");
        }
        return retVal.toString();
    }

    //--------------------------------------------------------------------------
    //  Inner class

}
