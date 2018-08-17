void setup() {
  Serial.begin(9600);
  delay(1000);
}

int i = 0;

void loop() {
  Serial.print("Temperature = ");
  Serial.print(0.01+i); // temperatura
  Serial.print(", Time = ");
  Serial.print(1.23+i); // czas symulacji
  Serial.println();
  Serial.print("Liquid Level = "); // poziom cieczy
  Serial.print(1.01+i);
  Serial.print(", Time = "); // czas symulacji
  Serial.print(4.23+i);
  Serial.println();
  i++;
  delay(1000);
}
