-- Skeletal program for Ada assignment at Programming Languages course
-- Students should rename tasks of producers, consumers, and the buffer
-- They should change them so that they will suit their own assignments
-- They should also supplement the code with missing constructions
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; 
with Ada.Numerics.Discrete_Random;

-- I prepared simulation of car manufactory, 4 production lines with 4 car type and 3 consumers

procedure Simulation is

   Number_of_Lines: constant Integer := 4; --number of production lines
   Number_of_Consumers: constant Integer := 3; --number of consumers
   
   subtype Product_Type is Integer range 1 .. Number_of_Lines;
   subtype Consumer_Type is Integer range 1 .. Number_of_Consumers;
   
   Car_name: constant array (Product_Type) of String(1 .. 9) -- different car types
     := ("SUV      ", "Limousine", "Sedan    ", "Combi    ");


    --DECLARATIONS--

   -- Production line productes specified part
   task type Production_line is
      -- Set up new production line
      entry Start(Line_number: in Product_Type); --line number, type of car
   end Production_line;

   -- Consumer gets a product consisting of parts from the Buffer
   task type Consumer is
      -- Set Consumer identity
      entry Start(Consumer_Number: in Consumer_Type); --consumer number
   end Consumer;

   -- In the Buffer, parts are assembled into products
   task type Buffer is
      -- Accept part into the warehouse
      entry Take(Car: in Product_Type; Number: in Integer; If_taken: out Boolean);
      -- Give out the product from the warehouse if there are enough parts
      entry Give(Consumer_number: in Consumer_Type; If_given: out Boolean);
   end Buffer;

   P: array ( 1 .. Number_of_Lines ) of Production_line;
   K: array ( 1 .. Number_of_Consumers ) of Consumer;
   B: Buffer;
 

    --DEFINITIONS--
   task body Production_line is
      subtype Production_Time_Range is Integer range 3 .. 6;
      package Random_production is new Ada.Numerics.Discrete_Random(Production_Time_Range);
      G: Random_production.Generator;	--  random generator
      Number_of_car: Integer;   -- number of choice
      Number_of_line: Product_Type; 
      Accepted_it: Boolean;
      
   begin
      accept Start(Line_number: in Product_Type) do
         Random_production.Reset(G);	--  random generator
         Number_of_car := 1;
         Number_of_line := Line_number; 
      end Start;
      
      Put_Line("L: Line number" & Integer'Image(Number_of_line) & " started, it will produce: " & Car_name(Number_of_line));
      loop
         delay Duration(Random_production.Random(G)); --  simulation of production
         Put_Line("L: Produced " & Car_name(Number_of_line)
                  & " number "  & Integer'Image(Number_of_car));         
               
         loop
            select
                 B.Take(Number_of_line, Number_of_car, Accepted_it);
                 Number_of_car := Number_of_car + 1;
                 if Accepted_it = True then
                    Put_Line("Line: received");
                    exit;
                else 
                    Put_Line("Line: Not received");
                end if;
            else
                Put_Line("BUFFER: Buffer is busy, please wait 5s.");
                delay 5.0;
            end select;
         end loop;
                  
        -- Number_of_car := Number_of_car + 1;
      end loop;
   end Production_line;
   
   
   ----CONSUMENT----
   task body Consumer is
      subtype Consumption_Time_Range is Integer range 4 .. 8;
      package Random_consumption is new Ada.Numerics.Discrete_Random(Consumption_Time_Range);
      G: Random_consumption.Generator;	--  random generator   
      Number_of_consumer: Consumer_Type;
      Released: Boolean;    
                  
   begin
      accept Start(Consumer_Number: in Consumer_Type) do
	 Random_consumption.Reset(G);	--  random generator started
	 Number_of_consumer := Consumer_Number;
      end Start;
      
      Put_Line("C: Consument number " &Integer'Image(Number_of_consumer) & " started");
      loop
	 delay Duration(Random_consumption.Random(G)); --  consumption simulation
	 
         loop 
            select 
                B.Give(Number_of_consumer, Released);            
                if Released = True then
                    Put_Line("Given");
                    exit;
				 else 
                    Put_Line("Not given");
                end if;	
            else
                Put_Line("BUFFER: ERROR");
                delay 10.0;    
            end select;
        end loop;
      end loop;
   end Consumer;
   
   
   ----BUFFER----
   exceptioon: exception;
   task body Buffer is
      Capacity_of_warehouses: constant Integer := 24;
      type Manufactory_warehouses is array (Product_Type) of Integer;
      Warehouse: Manufactory_warehouses:= (0, 0, 0, 0);
      Flota: array(Product_Type) of Integer:= (1, 1, 1, 1);
      Number_of_Flota: Integer := 1;	
      In_Warehouse: Integer := 0;
      
      
      procedure Warehouses_store(Mag: Manufactory_warehouses) is         
      begin
         for W in Product_Type loop
            Put_Line("|   Warehouse store: " & Integer'Image(Mag(W)) & " "
                     & Car_name(W));
         end loop;
         Put_Line("|   Number of cars in warehouse: " & Integer'Image(In_Warehouse));
      end Warehouses_store;


      function May_take(Car: Product_Type) return Boolean is
	 Free: Integer;		--  free slot in warehouse	 
	 Lack: array(Product_Type) of Integer;
	 -- number of sum lacks
         Lacks: Integer := 0;
         
      begin
	  
	  if Warehouse(Car) >=1 then
     raise exceptioon;
	  return false;
	  end if;
	  
	 if In_Warehouse >= Capacity_of_warehouses then
    raise exceptioon;
	    return False;  -- warehouse is overflowed
	 else
	    Free := Capacity_of_warehouses - In_Warehouse;           

            for W in Product_Type loop
               Lack(W) := Flota(W) - Warehouse(W);
               if Lack(W) > 0 then 
                     Lacks := Lacks + Lack(W);
               end if;
            end loop;               
            --if warehouse is near to overflowed and this car is not on the list of lacks we reject it
            if In_Warehouse+Lacks >= Capacity_of_warehouses and Lack(Car) <= 0 then                
               return False;            
            end if;            
         end if;           
         return True;
      end May_take;

      function May_give return Boolean is
      begin
	 for W in Product_Type loop
	    if Warehouse(W) < Flota(W) then
	       return False;
	    end if;
         end loop;         
	 return True;
      end May_give;
    

   begin
      Put_Line("B: Buffer started");
      loop
        
    select
         accept Take(Car: in Product_Type; Number: in Integer; If_taken: out Boolean) do
           Put_Line("B: Accepted  car "  & Car_name(Car) & " nr " &
		Integer'Image(Number));
	   if May_take(Car) then
	      Put_Line("B: " & Car_name(Car) & " accepted, nr " &
		Integer'Image(Number));
	      Warehouse(Car) := Warehouse(Car) + 1;
               In_Warehouse := In_Warehouse + 1;
               If_taken := True;
  	   else
	      Put_Line("B: " & Car_name(Car) & " rejected, nr " &
                  Integer'Image(Number)); 
               If_taken := False;
	   end if;
      exception
     when exceptioon =>
              Put_Line("BUFFER: Car not accepted, buffer is full!");

	 end Take;
         Warehouses_store(Warehouse);
         
       or    
         accept Give(Consumer_number: in Consumer_Type; If_given: out Boolean) do
            Put_Line("B: Accepted, give flota to consumer number " & Integer'Image(Consumer_Number));
            Warehouses_store(Warehouse);
	    if May_give then
               Put_Line("B: Flota number " & Integer'Image(Number_of_Flota) & "was given");               
	       for W in Product_Type loop
		  Warehouse(W) := Warehouse(W) - Flota(W);
		  In_Warehouse := In_Warehouse - Flota(W);
               end loop;               
               Number_of_Flota := Number_of_Flota + 1;
               If_given := True;
	    else
               Put_Line("B: Flota is not complete");	
               If_given := False;
	    end if;
	 end Give;
            Warehouses_store(Warehouse); 
            
        end select;
      end loop;
   end Buffer;
   
begin
   for I in 1 .. Number_of_Lines loop
      P(I).Start(I);
   end loop;
   for J in 1 .. Number_of_Consumers loop
      K(J).Start(J);
   end loop;
   
end Simulation;
