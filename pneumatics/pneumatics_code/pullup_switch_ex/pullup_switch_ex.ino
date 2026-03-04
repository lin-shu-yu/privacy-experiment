/* button switch testing script */
// Testing internal pull up resistor function, using Arduino built in LED to test button reads

// Author: Mich Lin
// Date created: Feb 14, 2026
// Date last modified: Feb 14, 2026


void setup() {

  //start serial connection
  Serial.begin(9600);

  //configure pin 2, 3, 4 as an input and enable the internal pull-up resistor
  // all digital pins have internal pull up resistors
  // internal pull up resister means: nominally open = HIGH/1; pushed close: LOW/0
  pinMode(2, INPUT_PULLUP);
  pinMode(3, INPUT_PULLUP);
  pinMode(4, INPUT_PULLUP);

// using built-in LED in 13 as testing
  pinMode(13, OUTPUT);

}

void loop() {

  //read the pushbutton value into a variable (0 or 1 - integer class)
  int button1 = digitalRead(2);
  int button2 = digitalRead(3);
  int button3 = digitalRead(4);

  //print out the values of the buttons
  Serial.print("button 1:");
  Serial.print(button1);
  Serial.print("button 2:");
  Serial.print(button2);
  Serial.print("button 3:");
  Serial.println(button3);


  if (button1 == LOW && button2 == LOW && button3 == LOW) { // turn on LED when all buttons are pushed = LOW
  
    digitalWrite(13, HIGH); 

  } else {

    digitalWrite(13, LOW); 

  }
}