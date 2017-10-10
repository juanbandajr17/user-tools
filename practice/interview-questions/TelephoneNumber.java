public class TelephoneNumber {
    private static final int PHONE_NUMBER_LENGTH = 7;
    private final int [] phoneNum;
    private char[] result= new char[PHONE_NUMBER_LENGTH];

    public TelephoneNumber(int[] n) { phoneNum = n; }
    public void printWords() { printWords(0); }

    private void printWords(int curDigit) {
        if (curDigit == PHONE_NUMBER_LENGTH) {
            System.out.println(new String(result));
            return;
        }

        for (int i = 1; i <= 3; ++i) {
            result[curDigit] = getCharKey(phoneNum[curDigit], i);
            printWords(curDigit + 1);

            if (phoneNum[curDigit] == 0 ||
                phoneNum[curDigit] == 1) return;
        }
    }
}


public class TelephoneNumber {
    private static final int PHONE_NUMBER_LENGTH = 7;
    private final int [] phoneNum;
    private char[] result = new char[PHONE_NUMBER_LENGTH];

    public TelephoneNumber(int[] n) { phoneNum = n; }

    public void printWords() {
        for(int i = 0; i < PHONE_NUMBER_LENGTH; ++i)
            result[i] = getCharKey(phoneNum[i], 1);

        for( ; ; ) {
            for(int i = 0; i < PHONE_NUMBER_LENGTH; ++i) {
                System.out.print(result[i]);
            }
            System.out.print('\n');

            for (int i = PHONE_NUMBER_LENGTH - 1; i >= -1; --i) {
                if (i == -1)
                    return;

                if (getCharKey(phoneNum[i], 3) == result[i] ||
                    phoneNum[i] == 0 || phoneNum[i] == 1) {
                    result[i] = getCharKey(phoneNum[i], 1);
                } else if (getCharKey(phoneNum[i], 1) == result[i]) {
                    result[i] = getCharKey(phoneNum[i], 2);
                    break;
                } else if (getCharKey(phoneNum[i], 2) == result[i]) {
                    result[i] = getCharKey(phoneNum[i], 3);
                    break;
                }
            }
        }
    }
}
