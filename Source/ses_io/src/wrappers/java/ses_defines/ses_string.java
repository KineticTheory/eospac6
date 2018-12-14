
package ses_defines;

public class ses_string {
	public String the_value;

        public ses_string () {
 	       the_value = new String();
	}
	public ses_string(String initial_value) {
	       the_value = new String(initial_value);
	}
        public void set_value(String set_value) {
	        if (the_value == null) {
	            the_value = set_value;
	        }
        }
}
