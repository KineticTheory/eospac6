


package ses_defines;

public class ses_label {
	public String the_value;

        public ses_label () {
	        the_value = new String();
	}
	public ses_label(String value) {
		the_value = new String(value);
	}
        public void set_value(String set_value) {
	        if (the_value == null) {
	            the_value = set_value;
	        }
        }
}
