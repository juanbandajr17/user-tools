bool reverseWords(char str[]) {
  char *buffer;
  int slen, tokenReaderPos, wordReadPos, wordEnd, writePos = 0;

  slen = strlen(str);
  tokenReadPos = slen - 1;
  buffer = (char *) malloc(slen+1);

  if(!buffer)
    return false;

  while(tokenRead >= 0) {
    if(str[tokenReadPos] == ' ') {
      buffer[writePos++] = str[tokenReadPos--];
    } else {
      wordEnd = tokenReadPos;
      while(tokenReadPos >= 0 && str[tokenReadPos] != ' ')
        tokenReadPos--;
      wordReadPos = tokenReadPos + 1;
      while(wordReadPos <= wordEnd) {
        buffer[writePos++] = str[wordReadPos++];
      }
    }
  }

  buffer[writePos] = '\0';
  strlcpy(str, buffer, slen + 1);
  free(buffer);
  return true;
}


bool wcReverseWords(wchar_t str[]) {
  int start = 0; end = 0; length;
  length = wcslen(str);

  wcReverseString(str, start, length-1);
  while(end < length) {
    if(str[end] != L' ') {
      start = end;
      while(end < length && str[end] != L' ')
        end++;
      end--;
      wcReverseString(str, start, end);
    }
    end++;
  }
}


void wcReverseString(wchar_t str[], int start, int end) {
  wchar_t temp;
  while(end > start) {
    temp = str[start];
    str[start] = str[end];
    str[end] = temp;
    start++; end--;
  }
}
