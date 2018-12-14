
package ses_defines;

public class ses_word_reference {

	public double[] the_value;

        public ses_word_reference(int the_size, double initial_value) {
		the_value = new double[the_size];
		for (int i = 0; i < the_size; i++) {
		    the_value[i] = initial_value;
		}

	}
        public ses_word_reference(int the_size) {
		the_value = new double[the_size];
		for (int i = 0; i < the_size; i++) {
		    the_value[i] = 0.0;
		}
        }
        public ses_word_reference(long the_size) {
	    the_value = new double[(int)the_size];
	    for (int i = 0; i < (int)the_size; i++) {
		    the_value[i] = 0.0;
		}
        }
}
