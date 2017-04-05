package helper.pdfpreprocessing.pdf.entity;

/**
 * Created by Aydinli on 04.04.2017.
 */
public class TableCell {

    //--------------------------------------------------------------------------
    //  Members
    private final String content;
    private final int idx;

    //--------------------------------------------------------------------------
    //  Initialization
    public TableCell(int idx, String content) {
        this.idx = idx;
        this.content = content;
    }

    //--------------------------------------------------------------------------
    //  Getter N Setter
    public String getContent() {
        return content;
    }

    public int getIdx() {
        return idx;
    }
    //--------------------------------------------------------------------------
    //  Method binding
    //--------------------------------------------------------------------------
    //  Implement N Override
    //--------------------------------------------------------------------------
    //  Utils
    //--------------------------------------------------------------------------
    //  Inner class
}
