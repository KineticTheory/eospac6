
package ses_defines;

public class ses_table_id_reference {
	public long[] the_value;
        public int the_size;

	public ses_table_id_reference(int my_size) {
		the_value = new long[my_size];
		the_size = my_size;
	}
	public ses_table_id_reference() {
   	        the_value = null;
                the_size = 0;
	}
}
