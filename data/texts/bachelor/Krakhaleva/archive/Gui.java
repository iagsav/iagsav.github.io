package pkg;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import javax.imageio.ImageIO;
import javax.swing.*;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;

public class Gui extends JFrame{
	public static String getFiles;
	public static String getFilef;
	public static String getFileR;
   
	public Gui(){
		
		super("Склеивание изображений");
		this.setLayout(null);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		this.setSize(500, 400);
		setVisible(true);
				
		JButton startButton = new JButton("Старт");
		add(startButton);
		startButton.setSize(130, 40);
		startButton.setLocation(5, 320);	
		
		JButton exitButton = new JButton("Выход");
		add(exitButton);
		exitButton.setSize(130, 40);
		exitButton.setLocation(350, 320);
		
		JButton chooseButton = new JButton("Выбрать изображение 1");
		add(chooseButton);
		chooseButton.setAlignmentX(CENTER_ALIGNMENT);
		chooseButton.setLocation(5, 220);
        chooseButton.setSize(200, 40);
     
        
        JButton chooseButton2 = new JButton("Выбрать изображение 2");
		add(chooseButton2);
		chooseButton2.setAlignmentX(CENTER_ALIGNMENT);
		chooseButton2.setLocation(280, 220);
        chooseButton2.setSize(200, 40);
        
        JButton chooseButtonR = new JButton("Выбрать Rscript.exe");
		add(chooseButtonR);
		chooseButtonR.setAlignmentX(CENTER_ALIGNMENT);
		chooseButtonR.setLocation(157, 320);
        chooseButtonR.setSize(170, 40);
 
        final JLabel label = new JLabel("Выбранный файл");
        label.setAlignmentX(CENTER_ALIGNMENT);
        label.setLocation(5, 170);
        label.setSize(200, 50);
        add(label);
        
        final JLabel label2 = new JLabel("Выбранный файл");
        label2.setAlignmentX(CENTER_ALIGNMENT);
        label2.setLocation(280, 170);
        label2.setSize(200, 50);
        add(label2);
        
       
		

		
        
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
        		ImageIcon imageIcon = new ImageIcon(getFilef);
     	       // ImgPanel pan = new ImgPanel(imageIcon.getImage());
     			//pan.setSize(200, 200);
     			//pan.setLocation(10, 10);
     	       // add(pan);
        		 try{    
        	            Process p = Runtime.getRuntime().exec(getFileR + " curs.R");

        	            int processComplete = p.waitFor();
        	           
        	            
        	               if (processComplete == 0) {
        	                    System.out.println("successfull");
        	                    File one = new File("new1.jpg");
        	            		File two = new File("new2.jpg");
        	            		BufferedImage img1 = ImageIO.read(one);
        	            		BufferedImage img2 = ImageIO.read(two);
        	            		BufferedImage img = new BufferedImage(img1.getWidth() + img2.getWidth(), Math.max(img1.getHeight(), img2.getHeight()), BufferedImage.TYPE_INT_ARGB);
        	            		Graphics g = img.createGraphics();
        	            		g.drawImage(img1, 0, 0, null);
        	            		g.drawImage(img2, img1.getWidth(), 0, null);
        	            		g.dispose();
        	            		
        	            		try {
        	            			ImageIO.write(img, "png", new File("newimg.png"));
        	            			 ImageIcon icon = new ImageIcon(img);
             	                    java.awt.Image image = icon.getImage(); // transform it         
             	                    java.awt.Image newimg = image.getScaledInstance(1000, 600,  java.awt.Image.SCALE_SMOOTH); // scale it the smooth way  
             	                    icon = new ImageIcon(newimg);
             	                    JOptionPane.showMessageDialog(null, icon, "Склееное изображение", JOptionPane.PLAIN_MESSAGE);
        	            		} catch (IOException ex) {
        	            			// TODO Auto-generated catch block
        	            			ex.printStackTrace();
        	            		}
  	                   
        	               } else {
        	                    System.out.println("Could not complete");
        	               }
        	            }
        		 
        	            catch (Exception ex)
        	            {
        	                ex.printStackTrace();
        	            }
        		
        	
        	}
        });
        
        chooseButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                JFileChooser fileopen = new JFileChooser();             
                int ret = fileopen.showDialog(null, "Открыть файл");                
                if (ret == JFileChooser.APPROVE_OPTION) {
                    File file = fileopen.getSelectedFile();
                    getFilef = file.getAbsolutePath();
                    label.setText(getFilef);         
                    try(FileWriter writer = new FileWriter("fF.txt", false))
                    {                  
                        writer.write(getFilef); 
                                                
                    }
                    catch(IOException ex){                         
                        System.out.println(ex.getMessage());
                    } 
                      
                }
            }
        });
        
        
   
        chooseButton2.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                JFileChooser fileopen = new JFileChooser();             
                int ret = fileopen.showDialog(null, "Открыть файл");                
                if (ret == JFileChooser.APPROVE_OPTION) {
                    File file = fileopen.getSelectedFile();
                    getFiles = file.getAbsolutePath();
                    label2.setText(getFiles);
                    try(FileWriter writer = new FileWriter("sF.txt", false))
                    {                  
                        writer.write(getFiles);                     
                    }
                    catch(IOException ex){                         
                        System.out.println(ex.getMessage());
                    } 
                    
                }
            }
        });
            
   
        
		 exitButton.addActionListener(new ActionListener() {
	        	public void actionPerformed(ActionEvent e) {
	        		System.exit(0);
	        	}
	        });
              
	}
	
/*class ImgPanel extends Component {
		
		private java.awt.Image img;
		
		public ImgPanel(java.awt.Image img) {
			this.img = img;
		}
		
		@Override
		public void paint(Graphics g) {
			g.drawImage(img, getX(), getY(), getWidth(), getHeight(), this);
		}
	}*/

}
