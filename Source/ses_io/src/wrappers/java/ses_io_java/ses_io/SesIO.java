// The ses_io.java file

package ses_io;

public class SesIO
{
    // Declaration of the Native (C) function
    public native long SesAccessDirectory(int the_handle, long[] matid, long[] nwds,long[] iadr, long date, long version);
    public native long SesAccessTableIndex(int the_handle, long[] tblid, long[] nwds, long[] iadr, long date1, long date2, long version);
    public native long SesArraySizeNext(int the_handle) throws NullPointerException;
    public native int SesChangeNext(int the_handle, double buffer[], long dim);
    public native int SesClose(int the_handle);
    public native int SesCombine(int handle1, int handle2, String new_filename);
    public native String SesComments(int handle);
    public native String SesDate(int the_handle);
    public native int SesDefineTable(int the_tid, String the_string, int num_arrays,
                                   long size_arrays[], Object labels[]);
    public native int SesDeleteNext(int the_handle);
    public native int SesDeleteTable(int the_handle);
    public native String SesGetLabel(int the_handle);
    public native int SesGetMaterialID(String material_name);
    public native long[] SesGetMaterials(int the_handle);
    public native long[] SesGetTableIDS(int the_handle, long mid);
    public native boolean SesHasNext(int the_handle);
    public native boolean SesIndicatesError(int the_error_flag);
    public native boolean SesIsValid(int the_handle);
    public native int SesOpen(String filename, char open_flags);
    public native String SesPrintErrorCondition(int the_handle);
    public native double[] SesRead1D(int the_handle, int  dim);
    public native double[] SesRead2D(int the_handle, int  dim1, int  dim2);
    public native double[] SesRead3D(int the_handle, int  dim1, int  dim2, int  dim3);
    public native double[]  SesReadNamedArray(int the_handle, String the_label, int dim);
    public native double[] SesReadNext(int the_handle, int size);
    public native long SesReadNumber(int the_handle);
    public native int SesReadPairs(int the_handle, double buf1[], double buf2[], long dim);
    public native double SesReadWord(int the_handle);
    public native int SesSetArrayOrder(int the_handle, char the_order);
    public native int SesSetDate(int the_handle, long the_date);
    public native int SesSetGrid(int the_handle, int nr, int nt, int ntab);
    public native int SesSetFormat(int the_handle, char the_format, char ascii_format);
    public native int SesSetLabel(int the_handle, String the_label);
    public native int SesSetMaterialOrder(int the_handle);
    public native int SesSetSignificantDigits(int the_handle, long number_digits);
    public native int SesSetup(int the_handle, int the_mid, int the_tid);
    public native int SesSetValidate(int the_handle);
    public native int SesSetVersion(int the_handle, long the_version);
    public native int SesSkip(int the_handle);
    public native String SesVersion(int the_handle);
    public native int SesWrite1D(int the_handle, double buf1[], int dim );
    public native int SesWrite2D(int the_handle, double buf1[], int dim1, int dim2);
    public native int SesWrite3D(int the_handle, double buf1[], int dim1, int  dim2, int dim3);
    public native int SesWriteComments(int the_handle, String the_comments, long dim1 );
    public native int SesWriteNext(int the_handle, double buf1[], int dim, String the_label);
    public native int SesWriteNumber(int the_handle, long the_number);
    public native int SesWritePairs(int the_handle, double buf1[], double buf2[], long dim);
    public native int SesWriteWord(int the_handle, double the_word );        



    static 
    {
      // The runtime system executes a class's static initializer 
      // when it loads the class.
      System.loadLibrary("sesj");
    }

    public SesIO() {
    }

 
}

