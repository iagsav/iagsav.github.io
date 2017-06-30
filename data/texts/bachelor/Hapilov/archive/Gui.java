package pkg;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;

import javax.imageio.ImageIO;
import javax.swing.Box;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;

import java.awt.Dimension;
import java.awt.Graphics;
public class Gui extends JFrame{
	public static String getFile;
	public static String getFileR;
	public String rScript;
	public Gui(){
		super("Распознование номерной пластины");
		this.setLayout(null);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		this.setSize(500, 350);
		setVisible(true);
		 rScript = "curs.R";
		
		JButton startButton = new JButton("Поиск!");
		add(startButton);
		startButton.setSize(100, 40);
		startButton.setLocation(0, 274);
		

		JButton exitButton = new JButton("Выход!");
		add(exitButton);
		exitButton.setSize(100, 40);
		exitButton.setLocation(384, 274);	
		
		JButton chooseButton = new JButton("Выбрать файл");
		add(chooseButton);
		chooseButton.setAlignmentX(CENTER_ALIGNMENT);
		chooseButton.setLocation(175, 190);
        chooseButton.setSize(160, 40);
        
        JButton chooseButtonR = new JButton("Выбрать Rscript.exe");
		add(chooseButtonR);
		chooseButtonR.setAlignmentX(CENTER_ALIGNMENT);
		chooseButtonR.setLocation(175, 240);
        chooseButtonR.setSize(160, 40);
 
        final JLabel label = new JLabel("Выбранный файл");
        label.setAlignmentX(CENTER_ALIGNMENT);
        label.setLocation(200, 150);
        label.setSize(150, 50);
        add(label);
 
        add(Box.createRigidArea(new Dimension(10, 10)));
        
        
        chooseButtonR.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                JFileChooser fileopenR = new JFileChooser();             
                int retR = fileopenR.showDialog(null, "Выбрать файл");                
                if (retR == JFileChooser.APPROVE_OPTION) {
                    File fileR = fileopenR.getSelectedFile();
                    getFileR = fileR.getAbsolutePath();
                    try(FileWriter writerR = new FileWriter("notes2.txt", false))
                    {                  
                        writerR.write(getFileR);                     
                    }
                    catch(IOException ex){                         
                        System.out.println(ex.getMessage());
                    } 
                    
                    
                }
            }
        });
        
        startButton.addActionListener(new ActionListener() {
        	public void actionPerformed(ActionEvent e) {
        		 try{    
        			
        	            Process p = Runtime.getRuntime().exec(getFileR + " curs.R");
        	            int processComplete = p.waitFor();
        	            System.out.println(processComplete);
        	            System.out.println(getFileR);
        	               if (processComplete == 0) {
        	                    System.out.println("successfull");
        	                    File fileImage = new File("NewImage.jpg");
        	            		BufferedImage img = ImageIO.read(fileImage);
        	            			ImageIcon icon = new ImageIcon(img);
             	                    java.awt.Image image = icon.getImage(); // transform it         
             	                    java.awt.Image newimg = image.getScaledInstance(1000, 800,  java.awt.Image.SCALE_SMOOTH); // scale it the smooth way  
             	                    icon = new ImageIcon(newimg);
             	                    JOptionPane.showMessageDialog(null, icon, "Найденный номер", JOptionPane.PLAIN_MESSAGE);
        	            		
        	               } 
        	               else {
        	            	   System.out.println(processComplete);
        	                    System.out.println("Could not complete");
        	              }
        	               
        	            }
        	            catch (Exception ex)
        	            {
        	                ex.printStackTrace();
        	            }
   
        	}
        });
        
        exitButton.addActionListener(new ActionListener() {
        	public void actionPerformed(ActionEvent e) {
        		System.exit(0); //Выход из программы
        	}
        });
        chooseButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                JFileChooser fileopen = new JFileChooser();             
                int ret = fileopen.showDialog(null, "Открыть файл");                
                if (ret == JFileChooser.APPROVE_OPTION) {
                    File file = fileopen.getSelectedFile();
                    getFile = file.getAbsolutePath();
                    label.setText(file.getName());
                    try(FileWriter writer = new FileWriter("path.txt", false))
                    {                  
                        writer.write(getFile);                     
                    }
                    catch(IOException ex){                         
                        System.out.println(ex.getMessage());
                    } 
                    
                }
            }
        });
      
	}

}
