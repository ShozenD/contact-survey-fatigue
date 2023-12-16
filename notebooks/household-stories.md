# Household Stories

```mermaid
flowchart LR

subgraph Wave 4
  Part2(Male 58) --> Cont3(Male 55-64)
end

subgraph Wave 1
  Part(Male 58)
  Cont1(Male 25-34)
  Cont2(Male 55-64)
  
  Part --> Cont1
  Part --> Cont2
end
```
- Participant is a 58 year old male, employed full-time
- In wave 1, he reported that his household comprised of two other men, one between 25-34 and another between 55-64, both of whom were employed full-time.
- In wave 4, the participant no longer reported that the younger man was part of the household.

```mermaid
flowchart LR

subgraph Wave 4
  Part2(Male 68) --> Cont3(Female 65-69)
end

subgraph Wave 1
  Part(Male 68)
  Cont1(Male 65-69)
  Cont2(Female 65-69)
  
  Part --> Cont1
  Part --> Cont2
end
```
- Participant is a 68 year old retired male
- In wave 1 he reported a household size of 3, comprised of two other retired people of the same age group. One of whom was male and the other female.
- In wave 4, the participant no longer reported that the male was part of the household.

new_id: **96ff4044**
```mermaid
flowchart LR

subgraph Wave 5
  Part4(Female 19)
  Cont7(Female 45-54)

  Part4 --> Cont7
end

subgraph Wave 3
  Part3(Female 19)
  Cont6(Male 20-24)

  Part3 --> Cont6
end

subgraph Wave 2
  Part2(Female 19)
  Cont3(Female 45-54)
  Cont4(Male 20-24)
  Cont5(Female 15-19)

  Part2 --> Cont3
  Part2 --> Cont4
  Part2 --> Cont5
end

subgraph Wave 1
  Part(Female 19)
  Cont1(Female 45-54)
  Cont2(Female 15-19)
  
  Part --> Cont1
  Part --> Cont2
end
```
- Participant is a 19 year old women.
- In the first wave she reported that her household size was 3, comprised of another women of the same age range and a women of age 45-54
- In the second wave a male of age 20-24 became part of the household
- In the third wave only the male was reported to be part of the household
- In the fifth wave only the women age 45-54 was reported to be part of the household

new_id: ****